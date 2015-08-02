package xxml1_1
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
  def exp:Parser[XXML] = (comment ^^ {Comment(_)}).
          | ((("<" ~> name <~ ">") ~ exps ~ ("<" ~> "/" ~> name <~ ">")).filter {case a~b~c =>a==c} ^^
            { case a~b~c => Block(a, b) } ).
          | ("<" ~> name <~ opt("/") <~ ">" ^^ { One(_) }).
          | (text ^^ {Text(_)}) 

  def exps = rep(exp)


  def parse(input: String) = parseAll(exps, input)
}

object main extends App {
  val res = XXMLParser.parse("<!--comme--nt--><p>ab<br> cd</p>")
  println("res="+res)
}
