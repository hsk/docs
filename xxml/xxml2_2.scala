package xxml2_2
import util.parsing.combinator._

trait XXML
case class Block(name:String,attr:Map[String,String], ls:List[XXML]) extends  XXML
case class Comment(s:String) extends XXML
case class One(name:String,attr:Map[String,String]) extends XXML
case class Text(s:String) extends XXML

object XXMLParser extends RegexParsers {
  def name = "[!:_a-zA-Z]+".r
  def text = "[^<]+".r
  def comment = "<!--" ~> rep("[^-]+".r | not("-->") ~> "-") <~ "-->" ^^
                {l=>l.foldLeft(""){(a,b)=>a+b}}
  def attr = name ~ opt("=" ~> ("\"([^\"]|\\.)+\"".r ^^ {b=>b.substring(1,b.length-1)} | "[^> ]+".r)) ^^ {
    case a~Some(b) => (a,b)
    case a~None => (a,a)
  }
  def attrs = rep(attr) ^^ {l => l.foldLeft(Map[String,String]()){case(a,b)=>a + b}}
  def exp:Parser[XXML] = (comment ^^ {Comment(_)}).
          | ((("<" ~> name ~ attrs <~ ">") ~ exps ~ ("<" ~> "/" ~> name <~ ">")).filter {case a~at~b~c =>a==c} ^^
            { case a~at~b~c => Block(a, at, b) } ).
          | ("<" ~> (name ~ attrs) <~ opt("/") <~ ">" ^^ { case a~b=>One(a,b) }).
          | (text ^^ {Text(_)}) 

  def exps = rep(exp)


  def parse(input: String) = parseAll(exps, input)
}

object main extends App {
  val res = XXMLParser.parse("<p a=\"test\" checked b=abc>ab<br> cd</p>")
  println("res="+res)
}
