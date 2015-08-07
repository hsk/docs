package xxml1_1

import fastparse.all._

trait XXML
case class Block(name:String, ls:Seq[XXML]) extends  XXML
case class Comment(s:String) extends XXML
case class One(name:String) extends XXML
case class Text(s:String) extends XXML

object XXMLParser {

  val name = P(CharIn("!:_", 'a' to 'z', 'A' to 'Z').rep(1).!)
  val comment = P( "<!--".! ~ ((!"-->" ~ AnyChar).rep.!) ~ "-->".! ).map{case(a,b,c)=>a+b+c}
  val text = P( (!"<" ~ AnyChar).rep(1).! )

  val exp:Parser[XXML] = (comment map {Comment(_)}).
          | (("<" ~ name.! ~ ">" ~ exps ~ "<" ~ "/" ~ name.! ~ ">").filter {case (a,b,c) =>a==c}.map
            { case (a,b,c) => Block(a, b) } ).
          | ("<" ~ name.! ~ ("/").? ~ ">" map { One(_) }).
          | (text map {Text(_)})

  def exps:Parser[Seq[XXML]] = P(exp.rep)

  def parse(inp:String) = (exps ~ End).parse(inp)

  val parseA = P( "a".!  )
}



object main extends App {
  println(XXMLParser.parseA.parse("a"))
  println(XXMLParser.comment.parse("<!--comme--nt-->"))
  println(XXMLParser.text.parse("abc:bbb<"))
  val res = XXMLParser.parse("<!--comme--nt--><p>ab<br> cd</p>")
  println("res="+res)
}
