package xxml3_2
import util.parsing.combinator._

trait XXML
case class Block(name:String, ls:List[XXML]) extends  XXML
case class Comment(s:String) extends XXML
case class One(name:String) extends XXML
case class Text(s:String) extends XXML
case class Lst(xs:List[XXML]) extends XXML

trait Se
case class Rep(a:Se) extends Se
case class Or(ses:List[Se]) extends Se
case class Tag(s:String, se:Se) extends Se
case class Var(s: String) extends Se
case class Schema(start:String, rules:Map[String, Se])

object XXMLParser extends RegexParsers {
  def name = "[!:_a-zA-Z]+".r
  def text = "[^<]+".r ^^ {Text(_)}
  def comment = "<!--" ~> rep("[^-]+".r | not("-->") ~> "-") <~ "-->" ^^
                {l=>l.foldLeft(""){(a,b)=>a+b}} ^^ {Comment(_)}
  def tag(name:String, p: =>Parser[XXML]):Parser[XXML] =
    "<" ~> name ~> ">" ~> rep(p) <~ opt("</" <~ opt(name) <~ ">") ^^ {Block(name, _)} | comment

  def exp = schema

  def schema =
    tag("schema",
      tag("start", text) |
      tag("rule",
        tag("name", text) |
        tag("call", text) |
        tag("rep", text) |
        tag("tag", tag("call", text) | tag("name", text))))

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
        case Block("schema",Block("start",List(Text(start)))::ls) =>
          Schema(start, rules(ls))
      }
    def rules(ls:List[XXML]):Map[String,Se] =
      ls.foldLeft(Map[String,Se]()) {
        case (env,Block("rule", Block("name",List(Text(name)))::ls)) =>
          env + (name -> Or(ors(ls)))
      }
    def ors(ls:List[XXML]):List[Se] =
      ls.map {
        case Block("call",List(Text(s))) => Var(s)
        case Block("rep",List(Text(s))) => Rep(Var(s))
        case Block("tag", Block("name",List(Text(name)))::ls) => Tag(name, Or(ors(ls)))
      }
    schema(xxml)
  }

  def parse(input: String) = {
    val r = parseAll(exp, input)
    r match {
      case Success(res, next) => res
      case NoSuccess(err, next) => throw new Exception(""+r)
    }
  }

  def compileSchema(input: String) = {
    val res = parse(input)
    val res2 = compile(res)
    compile(res2)
  }
}

object main extends App {
  val parser = XXMLParser.compileSchema("""
    <schema>
    <start>exps</>
    <rule>
      <name>exps</>
      <rep>exp</>
    <rule>
      <name>exp</>
      <call>table</>
      <call>text</>
    <rule>
      <name>table</>
      <tag>
        <name>table</>
        <call>tr</>
    <rule>
      <name>tr</>
      <tag>
        <name>tr</>
        <call>td</>
    <rule>
      <name>td</>
      <tag>
        <name>td</>
        <call>exp</>
    """)

  println(XXMLParser.parseAll(parser, """
    <table>
      <tr>
        <td>aaa
        <td>aaa
      <tr>
        <td>aaa
        <td>aaa
    </table>
    <table>
      <tr>
        <td>aaa
    """).get)
}
