package xxml
import util.parsing.combinator._

sealed trait XXML {
  def attrs(a:Map[String,String]):String = {
    a.toList match {
      case List() => ""
      case ls => ls.map{case(a,b)=>" "+a+"=\""+b+"\""}.mkString("")
    }
  }
  override def toString() = this match {
    case Block(name,a,ls) => "<"+name+attrs(a)+">"+ls.map(_.toString).mkString("")+"</"+name+">"
    case Comment(s) => "<!--"+s+"-->"
    case One(name,a) => "<"+name+attrs(a)+"/>"
    case Text(s) => s
    case Lst(xs) => xs.map(_.toString).mkString("")
  }
}
case class Block(name:String,attr:Map[String,String], ls:List[XXML]) extends  XXML
case class Comment(s:String) extends XXML
case class One(name:String,attr:Map[String,String]) extends XXML
case class Text(s:String) extends XXML
case class Lst(xs:List[XXML]) extends XXML

class XXMLParser extends RegexParsers {
  def name = "[!:_a-zA-Z0-9][:_a-zA-Z0-9]*".r
  def text = "[^<]+".r ^^ {Text(_)}
  def cdata(name:String) = ("[^<]+".r | not(("</"+name+"\\b").r) ~> "<").* ^^
    { case l=> Text(l.mkString("")) }

  def comment = "<!--" ~> ("[^-]+".r | not("-->") ~> "-").* <~ "-->" ^^
                {l=>Comment(l.foldLeft(""){(a,b)=>a+b})}

  def attr = name ~ opt("=" ~> ("\"([^\"]|\\.)+\"".r ^^ {b=>b.substring(1,b.length-1)} | "[^> ]+".r)) ^^ {
    case a~Some(b) => (a,b)
    case a~None    => (a,a)
  } | "\"([^\"]|\\.)+\"".r ^^ {b=>("",b.substring(1,b.length-1))}

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
  case class Rep1(a:Se) extends Se
  case class Or(ses:List[Se]) extends Se
  case class Tag(s:String, se:Se) extends Se
  case class Var(s: String) extends Se
  case class Not(a:Se,b:Se) extends Se
  case class Schema(start:String, rules:Map[String, Se])
  case class Seq(ls:List[Se]) extends Se
  case class Opt(a:Se) extends Se
  def toList(x:XXML):List[XXML] = {
    x match {
      case Lst(x) => x
      case x => List(x)
    }
  }
  def tag(name:String, p: =>Parser[XXML]):Parser[XXML] =
    ("<" ~> name.r ~> attrs <~ ">") ~ p <~ opt("</" ~ opt(name) ~ ">") ^^
    { case a~b => Block(name, a, toList(b)) } | comment
  def one(name:String):Parser[XXML] =
    ("<" ~> name.r ~ attrs <~ opt("/") <~ ">") ^^
    { case a~b => One(a, b) } | comment

  def schema:Parser[Schema] = rules ^^ { case (n,e)::rules => Schema(n,((n,e)::rules).toMap) case _ => null }
  def rules = rule.+

  def rule = (name <~ "::=") ~ e ^^ {case name~e => (name, e)}
  def e:Parser[Se] = repsep(app, "|") ^^ { case List(a)=> a case es => Or(es) }
  def app = rep1(sub) ^^ { case List(a) => a case s => Seq(s) }
  def sub = rep1sep(term, "-") ^^ { _.reduce{(a,b)=>Not(b,a)}}

  def term =
    fact ~ opt("*"|"+"|"?") ^^ {
      case a~Some("*") => Rep(a)
      case a~Some("+") => Rep1(a)
      case a~Some("?") => Opt(a)
      case a~None => a
    }
  def names = rep1sep(name,"|")
  def fact = name <~ not("::=") ^^ { Var(_) } |
    "(" ~> e <~ ")" |
    "{" ~> e <~ "}" ^^ { Rep(_)} |
    "[" ~> e <~ "]" ^^ { Opt(_)} |
    ("<" ~> names <~ "/>") ^^
    { case List(a) => Tag(a, Or(List())) case a => Or(a.map{Tag(_, Or(List()))}) } |
    (("<" ~> names <~ ">") ~ e ~ opt("<" ~> "/" ~> opt(names) <~ opt(">"))).filter
    { case a~b~Some(Some(c)) => a==c case _ => true } ^^
    { case List(a)~b~c => Tag(a, b) case a~b~c => Or(a.map{Tag(_, b)}) }

  def compile(schema:Schema): Parser[XXML] = {
    var env = scala.collection.mutable.Map[String, Parser[XXML]]("text"->text)
    def get(s: String): Parser[XXML] = 
      if(env.contains(s)) env(s) else
      new Parser[XXML] {
        lazy val parser = env(s)
        def apply(in: Input): ParseResult[XXML] = {
          parser(in)
        }
      }
    def comp(se:Se):Parser[XXML] =
      se match {
        case Rep(se) => comp(se).* ^^ {Lst(_)}
        case Rep1(se) => comp(se).+ ^^ {Lst(_)}
        case Or(s::ss) => ss.foldLeft(comp(s)){(a,b)=>a|comp(b)}
        case Or(Nil) => throw new Exception("error")
        case Tag(s,Or(List())) => one(s)
        case Tag(s,Var("cdata")) => tag(s,cdata(s))
        case Tag(s,se) => tag(s,comp(se))
        case Var(s) => get(s)
        case Not(a,b) => not(comp(a)) ~> comp(b)
        case Seq(s::ss) =>
          ss.foldLeft(comp(s)){(a,b)=>
            a ~ comp(b) ^^ {
              case a~b=>Lst(List(a):::toList(b))
            }
          }
        case Opt(s) => opt(comp(s)) ^^ {case None => Lst(List()) case Some(a) => Lst(List(a))}
          
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
    val res = parseSchema(input)
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
html    ::= (<html>[head] body) | body
head    ::= <head>
            { (<title>text)
            | <base|bgsound|isindex|nextid|command|link|meta/>
            | (<noscript>{inline})
            | script
            }
body    ::= (<body>{flow}) | {flow}
flow    ::= <hr/> | h1 | p
          | (<article|aside|blockquote|dfn|div|footer|form|header|nav|pre>{flow})
          | (<hgroup>{h1})
          | (<details>(<summary> inline) {flow})
          | (<fieldset>[<legend>inline] {flow})
          | (<figure> {flow | <figcaption>{flow}})
          | (<ol|ul>{<li>{flow}})
          | (<dl>{<dt|dd>flow})
          | (<table>
             [<caption>{flow}]
             {<colgroup>{<col>|script}}
             [<thead>{tr}]
             [<tfoot>{tr}]
             ((<tbody>{tr})|{tr})
             [<tfoot>{tr}]
            )
          | inline
h1      ::= <h1|h2|h3|h4|h5|h6>{flow - h1}
p       ::= <p>{inline - p}
tr      ::= <tr>{<td|th>{flow}}
inline  ::= text
          | (<area|link|meta|br|embed|img|input|keygen|wbr/>)
          | (<abbr|address|b|bdi|bdo|button|canv|cite|code|command
             |data|em|i|iframe|kbd|label|mark|noscript|output|q
             |s|samp|section|small|span|strong|sub|sup|svg|time|u
             |var|meter|progress
             >{inline})
          | (<datalist>{inline | option})
          | (<ruby>(<rt|rb>{inline}))
          | (<a|del|ins|map>{flow})
          | (<video|audio>{flow|<source|track/>})
          | script
          | (<textarea>cdata)
          | (<menu>{(<li>{flow})|flow})
          | (<object>{<param>}{flow})
          | (<select>{option|<optgroup>{option}})
option  ::= <option>text
script  ::= <script|template|style>cdata
    """)

    println("--------")


  println(XXMLSchema.parseAll(parser, """
<html>
<head><title>aaa</title>
<script>
function test(){
  return "</scrip>"
}
</script>
</head>
<body>
<h1>hoge
<h2>huga</>
<p a="1">huga<br><a href="hoge">aaa</a>
<p>hoge
<div>aaa
  <div>bbb
    <table>
      <tr>
        <td>aaa
    </table>
    <table>
      <tr>
        <td>aaa
        <td>aaa
    </table>
    <table>
      <tr>
        <td>aaa
  </div>
  hoge
</body>
</html>
    """))

  println(XXMLSchema.parseAll(parser, """
    <table>
      <tr>
        <td>aaa
        <td>aaa
    """))
}
