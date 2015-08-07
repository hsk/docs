package tes4
import scala.xml._
import util.parsing.combinator._
import util.parsing.input._

trait XXML {
  override def toString() = {
    this match {
      case Block(name,ls) => "<"+name+">"+ls.mkString("")+"</"+name+">"
      case Comment(s) => "<!--"+s+"-->"
      case Text(s) => s
    }
  }
}
case class Block(name:String, ls:List[XXML]) extends  XXML
case class Comment(s:String) extends XXML
case class Text(s:String) extends XXML

object XXMLParser extends RegexParsers with PackratParsers {
  lazy val name = "[!:_a-zA-Z]+".r
  lazy val cdata = "[^<]+".r ^^ {b=>List(Text(b))}
  lazy val comment = ("<!--" ~> rep("[^-]+".r | not("-->") ~> "-") <~ "-->" ^^
                {l=>l.foldLeft(""){(a,b)=>a+b}} ^^ {Comment(_)})
  def com(p:Parser[XXML]) = comment.* ~ p ~ comment.* ^^ {case a~b~c => a:::b::c }
  lazy val text = com("[^<\\s]+".r.+ ^^ {a=>Text(a.mkString(" "))})
  lazy val texts = (("[^<\\s]+".r.+ ^^ {a=>Text(a.mkString(" "))})|comment).*
  def r(p:Parser[List[XXML]]) = p.* ^^ {_.flatten} 
  def o(p:Parser[List[XXML]]) = p.? ^^ {case None => List() case Some(a)=>a}

  // 省略出来ないタグ
  def tag(name1:String, p: Parser[List[XXML]]) =
    com("<" ~> name1 <~ "/>" ^^ {Block(_, List())} |
        "<" ~> name1 ~> ">" ~> p <~ rep("</" ~ (not(name1 | ">")~name)  ~> ">") <~ opt("</" <~ opt(name1) <~ ">") ^^ {a=>Block(name1, a)})
  // 省略出来ないタグ一括登録
  def tags(names:List[String], p: Parser[List[XXML]]) = names.map{tag(_,p)}.reduce{_|_}
  // 閉じた具省略可能タグ
  def tago(name:String, p: Parser[List[XXML]]) =
    com("<" ~> name <~ "/>" ^^ {Block(_, List())} |
        "<" ~> name ~> ">" ~> p <~ opt("</" <~ opt(name) <~ ">") ^^ {Block(name, _)})
  // 一行タグ
  def one(name:String) = tago(name, comment.*)
  def ones(names:List[String]) = names.map{one(_)}.reduce{_|_}
  // 
  def otag(name:String, p: Parser[List[XXML]]) =
    tag(name, p) | (p ^^ {case b=> Block(name, b)::Nil})

  def parseHTML(input: String) =
    html(new PackratReader(new CharSequenceReader(input)))

  lazy val html:PackratParser[List[XXML]] = otag("html", head ~ body ^^ {case a~b=>a:::b})
  lazy val head:PackratParser[List[XXML]] = otag("head",r(
    ones(List("base","bgsound","isindex","nextid","command","link","meta")) |
    tag("title", texts) |
    tag("noscript", r(inline)) |
    script
  ))
  lazy val body:PackratParser[List[XXML]] = otag("body", r(flow))

  lazy val flow:PackratParser[List[XXML]] =
    one("hr") |
    tags(List("article","aside","blockquote","dfn","div","footer","form","header","nav","pre"),
      r(flow)) |
    tag("table",
        o(tag("caption", r(flow))) ~
        r(tag("colgroup", r(one("col") | script))) ~
        o(tago("thead", r(tr))) ~
        o(tag("tfoot", r(tr))) ~
        otag("tbody", r(tr)) ~
        o(tag("tfoot", r(tr))) ^^
        {case a~b~c~d~e~f=>List(a,b,c,d,e,f).flatten}
    ) |
    tago("dl", r( tago("dt", r(flow)) | tag("dd", r(flow)) )) |
    tags(List("ol","ul"), r(tago("li", r(flow)))) |
    tag("hgroup", r(heading)) |
    tag("fieldset", o(tag("legend", r(inline))) ~ r(flow) ^^ {case a~b=>a:::b}) |
    tag("details", tag("summary", r(inline)) ~ r(flow) ^^ {case a~b=>a:::b}) |
    tag("figure", r(flow | tag("figcaption", r(flow)))) |
    heading | p | inline
  lazy val heading:Parser[List[XXML]] =
    tags(List("h1","h2","h3","h4","h5","h6"), r(not(heading) ~> flow))
  lazy val p:Parser[List[XXML]] = tago("p", r(not(p) ~> inline))
  lazy val tr = tago("tr",r(tago("td", r(flow)) | tago("th", r(flow))))

  lazy val inline:Parser[List[XXML]] = text |
    ones(List("area", "link", "meta", "br", "embed", "img", "input", "keygen", "wbr")) |
    tags(List("abbr", "address", "b", "bdi", "bdo", "button", "canv", "cite",
      "code", "command", "data", "em", "i", "iframe", "kbd", "label", "mark",
      "noscript", "output", "q", "s", "samp", "section", "small", "span",
      "strong", "sub", "sup", "svg", "time", "u", "var", "meter", "progress"),
      r(inline)) |
    tags(List("a", "del", "ins", "map"), r(flow)) |
    tag("textarea", cdata) |
    tag("select", r(option | tag("optgroup", r(option)) ) ) | 
    tag("datalist", r(inline | option)) |
    tag("menu", r(tago("li", r(flow)) | flow) ) |
    tag("ruby", r(tag("rt", r(inline)) | tag("rt", r(inline)))) |
    tag("object", r(one("param")) ~ r(flow) ^^ {case a~b=>a:::b} ) |
    tags(List("video","audio"), r(flow | ones(List("source", "track")))) |
    script

  lazy val option = tago("option", texts)
  lazy val script = tags(List("script", "template", "style"), cdata)
}

object main extends App {
  println(XXMLParser.parseHTML("<!--test--><table><tr><td>aa<tr/></table>"))
  println(XXMLParser.parseHTML("<div><table><tr><td>hajime<tr/></div>aa</div><table><tr><td>owari</table>gege"))
  println(XXMLParser.parseHTML("<html><body><hr/><table><tr><td>aa<tr/></table></body></html>"))
  println(XXMLParser.parseHTML("<html><head><title>aaa</title></head><body><hr/><table><tr><td>aa<tr/></table></body></html>"))
}

object main2 extends App {
  def time(any: => Any) {
    for(i <- 0 until 10) {
      val start = System.currentTimeMillis
      any
      println(System.currentTimeMillis - start)
    }
  }
  time {
      for(i <- 0 to 1000)
        XXMLParser.parseHTML("<body><hr/><table><tr><td>aa<tr/></table></body>")
  }

}
