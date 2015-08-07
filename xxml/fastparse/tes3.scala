package tes3
import scala.xml._
import fastparse.all._
import scala.collection.mutable.ArrayBuffer

trait XXML {
  override def toString() = {
    this match {
      case Block(name,ls) => '<'+name+'>'+ls.mkString("")+"</"+name+'>'
      case Comment(s) => "<!--"+s+"-->"
      case Text(s) => s
    }
  }
}
case class Block(name:String, ls:Seq[XXML]) extends  XXML
case class Comment(s:String) extends XXML
case class Text(s:String) extends XXML

object XXMLParser {

  lazy val name = CharIn("!:_", 'a' to 'z', 'A' to 'Z').rep(1).!
  lazy val cdata = (!"<" ~ AnyChar).rep(1).! map {b=>ArrayBuffer(Text(b))}
  lazy val comment = "<!--" ~ (!"-->" ~ AnyChar).rep.! ~ "-->"  map{Comment(_)}

  def com(p:Parser[XXML]):Parser[Seq[XXML]] = comment.rep ~ p ~ comment.rep map
    {case (a,b,c) => a++Some(b)++c }

  lazy val text:Parser[Seq[XXML]] = ( (!"<" ~ AnyChar).rep(1).! ) map {b=>ArrayBuffer(Text(b))}
  lazy val txt:Parser[XXML] = ( (!"<" ~ AnyChar).rep(1).! ) map {b=>Text(b)}
  lazy val texts:Parser[Seq[XXML]] = (txt | comment).rep

  def r(p:Parser[Seq[XXML]]):Parser[Seq[XXML]] = p.rep map {_.flatten}
  def o(p:Parser[Seq[XXML]]) = p.? map {case None => ArrayBuffer() case Some(a)=>a}

  // 省略出来ないタグ
  def tag(name1:String, p: Parser[Seq[XXML]]):Parser[Seq[XXML]] =
    com(
      (("<" + name1) ~ "/>") .map {Unit=>Block(name1, ArrayBuffer())} |
      (("<" + name1) ~ ">" ~ p ~ ("</" ~ (!(name1 | ">")~name) ~ ">").rep ~
       ("</" ~ name1.? ~ ">").?).map {case(a,b)=>Block(name1, a)}
    )

  // 省略出来ないタグ一括登録
  def tags(names:ArrayBuffer[String], p: Parser[Seq[XXML]]):Parser[Seq[XXML]] =
    names.map{tag(_,p)}.reduce{_|_}

  // 閉じた具省略可能タグ
  def tago(name:String, p: Parser[Seq[XXML]]) =
    com((("<" + name).! ~ "/>") .map {a=>Block(name, ArrayBuffer())} |
        (("<" + name) ~ ">" ~ p ~ ("</" ~ name.? ~ ">").?) .map {Block(name, _)})
  // 一行タグ
  def one(name:String) = tago(name, comment.rep)
  def ones(names:ArrayBuffer[String]) = names.map{one(_)}.reduce{_|_}

  // 
  def otag(name:String, p: Parser[Seq[XXML]]) =
    tag(name, p) | (p map {b=> Block(name, b)::Nil})

  def parseHTML(input: String) = html.parse(input)

  lazy val html = otag("html", head ~ body map {case (a,b)=>a++b})
  lazy val head = otag("head",r(
    ones(ArrayBuffer("base","bgsound","isindex","nextid","command","link","meta")) |
    tag("title", texts) |
    tag("noscript", r(inline)) |
    script
  ))
  lazy val body = otag("body", r(flow))

  lazy val flow:Parser[Seq[XXML]] = P(
    one("hr") |
    tags(ArrayBuffer("article","aside","blockquote","dfn","div","footer","form","header","nav","pre"),
      r(flow)) |

    tag("table",
        o(tag("caption", r(flow))) ~
        r(tag("colgroup", r(one("col") | script))) ~
        o(tago("thead", r(tr))) ~
        o(tag("tfoot", r(tr))) ~
        otag("tbody", r(tr)) ~
        o(tag("tfoot", r(tr))) map
        {case (a,b,c,d,e,f)=>ArrayBuffer(a,b,c,d,e,f).flatten}
    ) |
    tago("dl", r( tago("dt", r(flow)) | tag("dd", r(flow)) )) |
    tags(ArrayBuffer("ol","ul"), r(tago("li", r(flow)))) |
    tag("hgroup", r(heading)) |
    tag("fieldset", o(tag("legend", r(inline))) ~ r(flow) map {case (a,b)=>a++b}) |
    tag("details", tag("summary", r(inline)) ~ r(flow) map {case (a,b)=>a++b}) |
    tag("figure", r(flow | tag("figcaption", r(flow)))) |
    heading | p | inline)

  lazy val heading:Parser[Seq[XXML]] = P(
    tags(ArrayBuffer("h1","h2","h3","h4","h5","h6"), r(!heading ~ flow)))

  lazy val p:Parser[Seq[XXML]] = P(tago("p", r(!p ~ inline)))
  lazy val tr = tago("tr",r(tago("td", r(flow)) | tago("th", r(flow))))
  lazy val inline:Parser[Seq[XXML]] = P(text |
    ones(ArrayBuffer("area", "link", "meta", "br", "embed", "img", "input", "keygen", "wbr")) |
    tags(ArrayBuffer("abbr", "address", "b", "bdi", "bdo", "button", "canv", "cite",
      "code", "command", "data", "em", "i", "iframe", "kbd", "label", "mark",
      "noscript", "output", "q", "s", "samp", "section", "small", "span",
      "strong", "sub", "sup", "svg", "time", "u", "var", "meter", "progress"),
      r(inline)) |
    tags(ArrayBuffer("a", "del", "ins", "map"), r(flow)) |
    tag("textarea", cdata) |
    tag("select", r(option | tag("optgroup", r(option)) ) ) | 
    tag("datalist", r(inline | option)) |
    tag("menu", r(tago("li", r(flow)) | flow) ) |
    tag("ruby", r(tag("rt", r(inline)) | tag("rt", r(inline)))) |
    tag("object", r(one("param")) ~ r(flow) map {case (a,b)=>a++b} ) |
    tags(ArrayBuffer("video","audio"), r(flow | ones(ArrayBuffer("source", "track")))) |
    script)

  lazy val option = tago("option", texts)
  lazy val script = tags(ArrayBuffer("script", "template", "style"), cdata)

}

object main extends App {

  println(XXMLParser.parseHTML("<!--test--><table><tr><td>aa<tr/></table>"))
  println(XXMLParser.parseHTML("<div><table><tr><td>hajime<tr/></div>aa</div><table><tr><td>owari</table>gege"))
  println(XXMLParser.parseHTML("<html><body><hr/><table><tr><td>aa<tr/></table></body></html>"))
  println(XXMLParser.parseHTML("<html><head><title>aaa</title></head><body><hr/><table><tr><td>aa<tr/></table></body></html>"))

}

object main2 extends App {
  println(XXMLParser.parseHTML("<!--test--><table><tr><td>aa<tr/></table>"))
  println(XXMLParser.parseHTML("<div><table><tr><td>hajime<tr/></div>aa</div><table><tr><td>owari</table>gege"))
  println(XXMLParser.parseHTML("<html><body><hr/><table><tr><td>aa<tr/></table></body></html>"))
  println(XXMLParser.parseHTML("<html><head><title>aaa</title></head><body><hr/><table><tr><td>aa<tr/></table></body></html>"))
  println(XXMLParser.parseHTML("<!--test--><hr><div>aa</div>"))
  println(XXMLParser.name.parse("abc"))

  def time(any: => Any) {
    for(i <- 0 until 10) {
      val start = System.currentTimeMillis
      any
      println(System.currentTimeMillis - start)
    }
  }
  println(XXMLParser.parseHTML("<body><hr/><table><tr><td>aa<tr/></table></body>"))

  time {
      for(i <- 0 to 1000)
        XXMLParser.parseHTML("<body><hr/><table><tr><td>aa<tr/></table></body>")
  }

}
