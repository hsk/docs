package xxml1_3
import util.parsing.combinator._

trait XXML
case class Block(name:String, ls:List[XXML]) extends  XXML
case class Comment(s:String) extends XXML
case class One(name:String) extends XXML
case class Text(s:String) extends XXML
case class Lst(ts:List[XXML]) extends XXML
object XXMLParser extends RegexParsers {
  def name = "[!:_a-zA-Z]+".r
  def text = "[^<]+".r
  def comment = "<!--" ~> rep("[^-]+".r | not("-->") ~> "-") <~ "-->" ^^
                {l=>l.foldLeft(""){(a,b)=>a+b}}

  var tags:List[String] = List()
  def tagBlock = (("<" ~> name <~ ">").filter{ blockTags.contains(_) } ~
  exps ~
  opt("<"~>"/"~>name<~">")) .filter{case a~b~Some(c)=> a == c case _ => true} ^^
  { case n~t~None =>
      var pos = t.indexWhere{case Block(n2,_) if(n2==n) => true case _ => false}
      if (pos < 0)
        Block(n, t)
      else {
        val (t1, t2) = t.splitAt(pos)
        Lst(Block(n,t1)::t2)
      }
    case n~t~n2=> Block(n,t)
  }
  def exp:Parser[XXML] = (comment ^^ {Comment(_)}).
          | (tagBlock).
          | ("<" ~> name <~ opt("/") <~ ">" ^^ { One(_) }).
          | (text ^^ {Text(_)}) 

  def exps = rep(exp) ^^ {ts =>
    ts.foldRight(List[XXML]()){
      case (Lst(a),b)=>a:::b
      case (a,b)=>a::b
    }

  }


  def parse(input: String) = parseAll(exps, input)
  var blockTags = Set("table","tr","td")
}

object main extends App {
  val res = XXMLParser.parse("<table><tr><td>a<td>a<tr><td>a<td>a")
  println("res="+res)
}
