package xxml
import util.parsing.combinator._

trait XXML
case class Block(name:String,attr:Map[String,String], ls:List[XXML]) extends  XXML
case class Comment(s:String) extends XXML
case class One(name:String,attr:Map[String,String]) extends XXML
case class Text(s:String) extends XXML
case class Lst(xs:List[XXML]) extends XXML

class XXMLParser extends RegexParsers {
  def name = "[!:_a-zA-Z0-9][:_a-zA-Z0-9]*".r
  def text = "[^<]+".r ^^ {Text(_)}
  def comment = "<!--" ~> ("[^-]+".r | not("-->") ~> "-").* <~ "-->" ^^
                {l=>Comment(l.foldLeft(""){(a,b)=>a+b})}

  def attr = name ~ opt("=" ~> ("\"([^\"]|\\.)+\"".r ^^ {_.substring(1)} | "[^> ]+".r)) ^^ {
    case a~Some(b) => (a,b)
    case a~None    => (a,a)
  } | "\"([^\"]|\\.)+\"".r ^^ {b=>("",b.substring(1))}

  def attrs = (attr).* ^^ {l => l.foldLeft(Map[String,String]()){case(a,b)=>a + b}}

  def tagText = ("<" ~> (name ~ "!" ~ attrs) <~ ">") ~ ("[^<]+".r | not("</") ~> "<").* ~
    opt("<"~>"/"~>opt(name)<~opt(">")) ^^
    { case n~m~a~t~n2=> Block(n+m,a,List(Text(t.foldLeft(""){_+_})))}

  def block = (("<" ~> name ~ attrs <~ ">") ~ elements ~ ("<" ~> "/" ~> opt(name) <~ opt(">"))).filter
    { case a~at~b~Some(c) => a==c case _ => true } ^^
    { case a~at~b~c       => Block(a, at, b) }

  def one = "<" ~> (name ~ attrs) <~ opt("/") <~ ">" ^^ { case a~b=>One(a,b) }

  def element:Parser[XXML] = comment | tagText | block | one | text 
  def elements = element.*
  def parse(input: String) = parseAll(elements, input)
}

object XXMLParser extends XXMLParser {
}

object XXMLSchema extends XXMLParser {
  sealed trait Se
  case class Rep(a:Se) extends Se
  case class Or(ses:List[Se]) extends Se
  case class Tag(s:String, se:Se) extends Se
  case class Var(s: String) extends Se
  case class Schema(start:String, rules:Map[String, Se])

  def toList(x:XXML):List[XXML] = {
    x match {
      case Lst(x) => x
      case x => List(x)
    }
  }
  def tag(name:String, p: =>Parser[XXML]):Parser[XXML] =
    ("<" ~> name.r ~> attrs <~ ">") ~ p <~ opt("</" ~ opt(name) ~ ">") ^^
    { case a~b => Block(name, a, toList(b)) } | comment

  def schema:Parser[Schema] = rules ^^ { case (n,e)::rules => Schema(n,((n,e)::rules).toMap) case _ => null }
  def rules = rule.+

  def rule = (name <~ "=") ~ e ^^ {case name~e => (name, e)}
  def e:Parser[Se] = repsep(term, "|") ^^ { case List(a)=> a case es => Or(es) }
  def term = fact <~ "*" ^^ { Rep(_) } | fact
  def fact = name ^^ { Var(_) } | "(" ~> e <~ ")" | "{" ~> e <~ "}" ^^ { Rep(_)} | 
    (("<" ~> name <~ ">") ~ e ~ opt("<" ~> "/" ~> opt(name) <~ opt(">"))).filter
    { case a~b~Some(Some(c)) => a==c case _ => true } ^^
    { case a~b~c       => Tag(a, b) }

  def compile(schema:Schema): Parser[XXML] = {
    var env = scala.collection.mutable.Map[String, Parser[XXML]]("text"->text)
    def get(s: String): Parser[XXML] = new Parser[XXML] {
      def apply(in: Input): ParseResult[XXML] = {
        env(s)(in)
      }
    }
    def comp(se:Se):Parser[XXML] =
      se match {
        case Rep(se) => comp(se).* ^^{Lst(_)}
        case Or(s::ss) => ss.foldLeft(comp(s)){(a,b)=>a|comp(b)}
        case Or(Nil) => throw new Exception("error")
        case Tag(s,se) => tag(s,comp(se))
        case Var(s) => get(s)
      }
    schema.rules.foreach {
      case(name,se)=> env += (name -> comp(se))
    }
    env(schema.start)
  }

  def parseSchema(input: String):Schema = {
    val r = parseAll(schema, input)
    r match {
      case Success(res, next) => res
      case NoSuccess(err, next) => throw new Exception(""+r)
    }
  }

  def compileSchema(input: String) = {
    val res:Schema = parseSchema(input)
    println(res)
    compile(res)
  }
}

object main extends App {
  val res = XXMLParser.parse("""
    <!DOCTYPE html>
    <class hoge>
      <def add a b>
        <add><a><b></add>
      </>
      <def main>
        <print><add><1><2></add></>
      </>
    </>
    """)
  println("res="+res)

  val parser = XXMLSchema.compileSchema("""
    exps  = (table | text)*
    table = <table>tr*
    tr    = <tr>td*</> | <th>td*
    td    = <td>exps
    """)

  println(XXMLSchema.parseAll(parser, """
      <table>
        <th>
          <td>aaa
          <td>aaa
        <tr>
          <td>aaa
          <td>aaa
      </table>
      <table>
        <tr>
          <td>aaa
          <td>aaa
        <tr>
          <td>aaa
          <td>aaa
    """))
}
