package xxml3_1
import util.parsing.combinator._

trait XXML
case class Block(name:String, ls:List[XXML]) extends  XXML
case class Comment(s:String) extends XXML
case class One(name:String) extends XXML
case class Text(s:String) extends XXML
case class Lst(xs:List[XXML]) extends XXML

object XXMLParser extends RegexParsers {
def name = "[!:_a-zA-Z]+".r
def text = "[^<]+".r ^^ {Text(_)}
def comment = "<!--" ~> rep("[^-]+".r | not("-->") ~> "-") <~ "-->" ^^
              {l=>l.foldLeft(""){(a,b)=>a+b}} ^^ {Comment(_)}
def tag(name:String, p: =>Parser[XXML]):Parser[XXML] =
  "<" ~> name ~> ">" ~> rep(p) <~ opt("</" <~ opt(name) <~ ">") ^^ {Block(name, _)} | comment

  def exps = rep1(exp)
  def exp = table | text

  def table = tag("table", tr) 
  def tr = tag("tr", td)
  def td:Parser[XXML] = tag("td", exp)

  def parse(input: String) = parseAll(exps, input)
}

object main extends App {
  val res = XXMLParser.parse("""
<!--test-->
<table>
  <tr>
    <td>aaa
    <td>aaa
  <tr>
    <td>gaga
    <td>gaga
</table>
<table>
  <tr>
    <td>aa
""")
  println("res="+res)
}
