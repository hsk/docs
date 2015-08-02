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
  def comment = "<!--" ~> rep("[^-]+".r | not("-->") ~> "-") <~ "-->" ^^
                {l=>Comment(l.foldLeft(""){(a,b)=>a+b})}
  def attr = name ~ opt("=" ~> ("\"([^\"]|\\.)+\"".r ^^ {b=>b.substring(1,b.length-1)} | "[^> ]+".r)) ^^ {
    case a~Some(b) => (a,b)
    case a~None => (a,a)
  } | "\"([^\"]|\\.)+\"".r ^^ {b=>("",b.substring(1,b.length-1))}

  def attrs = rep(attr) ^^ {l => l.foldLeft(Map[String,String]()){case(a,b)=>a + b}}
  def tagText = ("<" ~> (name ~ "!" ~ attrs) <~ ">") ~ rep("[^<]+".r | not("</") ~> "<") ~
    opt("<"~>"/"~>opt(name)<~opt(">")) ^^
    { case n~m~a~t~n2=> Block(n+m,a,List(Text(t.foldLeft(""){_+_})))}
  def one = "<" ~> (name ~ attrs) <~ opt("/") <~ ">" ^^ { case a~b=>One(a,b) }
  def exp:Parser[XXML] = (comment).
          | (tagText).
          | ((("<" ~> name ~ attrs <~ ">") ~ exps ~ ("<" ~> "/" ~> opt(name) <~ opt(">"))).
            filter {case a~at~b~Some(c) =>a==c case _ => true} ^^
            { case a~at~b~c => Block(a, at, b) } ).
          | (one).
          | (text) 
  def exps = rep(exp)
  def parse(input: String) = parseAll(exps, input)
}
object XXMLParser extends XXMLParser{}

object XXMLSchema extends XXMLParser {
  trait Se
  case class Rep(a:Se) extends Se
  case class Or(ses:List[Se]) extends Se
  case class Tag(s:String, se:Se) extends Se
  case class Var(s: String) extends Se
  case class Schema(start:String, rules:Map[String, Se])

  def tag(name:String, p: =>Parser[XXML]):Parser[XXML] =
    ("<" ~> (name ).r ~> attrs <~ ">") ~ rep(p) <~ opt("</" <~ opt(name) <~ ">") ^^
    {case a~b=>Block(name, a, b)} | comment

  def one2 =
    not("<![:_a-zA-Z0-9]*".r ~ attrs ~ opt("/") ~ ">") ~> one

  def schema =
    tag("!RULES",
      tag("!",
        tag("!l", text) |
        tag("!t", one2) |
        one2
      )
    )

  def compile(schema:Schema): Parser[XXML] = {
    var env = scala.collection.mutable.Map[String, Parser[XXML]]("text"->text)
    def get(s: String): Parser[XXML] = new Parser[XXML] {
      def apply(in: Input): ParseResult[XXML] = {
        env(s)(in)
      }
    }
    def comp(se:Se):Parser[XXML] =
      se match {
        case Rep(se) => rep(comp(se))^^{Lst(_)}
        case Or(s::ss) => ss.foldLeft(comp(s)){(a,b)=>a|comp(b)}
        case Tag(s,se) => tag(s,comp(se))
        case Var(s) => get(s)
      }
    schema.rules.foreach {
      case(name,se)=> env += (name -> comp(se))
    }
    env(schema.start)
  }

  def compile(xxml:XXML):Schema = {
    def schema(xxml:XXML):Schema =
      xxml match {
        case Block("!RULES",m,ls) =>
          Schema(m.toList.head._1, rules(ls))
      }
    def rules(ls:List[XXML]):Map[String,Se] =
      ls.foldLeft(Map[String,Se]()) {
        case (env,Block("!", m, ls)) =>
          env + (m.toList.head._1 -> Or(ors(ls)))
      }
    def ors(ls:List[XXML]):List[Se] =
      ls.map {
        case Block("!l", m, _) => Rep(Var(m.toList.head._1))
        case Block("!t", m, ls) => Tag(m.toList.head._1, Or(ors(ls)))
        case One(name,_) => Var(name)
      }
    schema(xxml)
  }

  def parseSchema(input: String) = {
    val r = parseAll(schema, input)
    r match {
      case Success(res, next) => res
      case NoSuccess(err, next) => throw new Exception(""+r)
    }
  }

  def compileSchema(input: String) = {
    val res = parseSchema(input)
    println(res)
    val res2 = compile(res)
    compile(res2)
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
    <!RULES es>
    <! es><!l e>
    <! e><tb><text>
    <! tb><!t table><tr>
    <! tr><!t tr><td><!t th><td>
    <! td><!t td><e>
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
    """))
}
