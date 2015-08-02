package xxml1_2
import util.parsing.combinator._

trait XXML
case class Block(name:String, ls:List[XXML]) extends  XXML
case class Comment(s:String) extends XXML
case class One(name:String) extends XXML
case class Text(s:String) extends XXML

object XXMLParser extends RegexParsers {
  def name = "[!:_a-zA-Z]+".r
  def text = "[^<]+".r
  def comment = "<!--" ~> rep("[^-]+".r | not("-->") ~> "-") <~ "-->" ^^
                {l=>l.foldLeft(""){(a,b)=>a+b}}

  var tags:List[String] = List()
  def tagText = ("<" ~> name <~ ">").filter{
    case n => if (textTags.contains(n)) { tags = n :: tags; true } else false
  } ~ rep("[^<]+".r | not("<" ~> "/" ~> tags.head) ~> "<") ~
  opt("<"~>"/"~>name<~">") ^^
  { case n~t~n2=> Block(n,List(Text(t.foldLeft(""){_+_})))}

  def exp:Parser[XXML] = (comment ^^ {Comment(_)}).
          | (tagText).
          | ((("<" ~> name <~ ">") ~ exps ~ ("<" ~> "/" ~> name <~ ">")).filter {case a~b~c =>a==c} ^^
            { case a~b~c => Block(a, b) } ).
          | ("<" ~> name <~ opt("/") <~ ">" ^^ { One(_) }).
          | (text ^^ {Text(_)}) 

  def exps = rep(exp)


  def parse(input: String) = parseAll(exps, input)
  var textTags = Set("script","textarea","style")
}

object main extends App {
  val res = XXMLParser.parse("<!--comme--nt--><p>ab<br> cd</p><script><p>test</p></script>")
  println("res="+res)
}
