package tes
import scala.xml._
import util.parsing.combinator._

trait XXML
case class Block(name:String, ls:XXML) extends  XXML
case class Comment(s:String) extends XXML
case class One(name:String) extends XXML
case class Text(s:String) extends XXML
case class Lst(xs:List[XXML]) extends XXML


object XXMLParser extends RegexParsers {


  def name = "[!:_a-zA-Z]+".r
  def text = "[^<]+".r ^^ {Text(_)}
  def cdata = "[^<]+".r ^^ {Text(_)}
  def comment = "<!--" ~> rep("[^-]+".r | not("-->") ~> "-") <~ "-->" ^^
                {l=>l.foldLeft(""){(a,b)=>a+b}} ^^ {Comment(_)}
  def tag(name:String, p: =>Parser[XXML]):Parser[XXML] =
    "<" ~> name ~> "/>" ^^ {a=>Block(name, Lst(List()))} |
    "<" ~> name ~> ">" ~> p <~ opt("</" <~ opt(name) <~ ">") ^^ {Block(name, _)} | comment

  def one(name:String):Parser[XXML] =
    "<" ~> name ~> "/>" ^^ {a=>Block(name, Lst(List()))} | comment

  def schema =
    tag("schema",
      tag("start", text) |
      tag("rule",
        tag("name", text) |
        tag("call", text) |
        tag("rep", text) |
        tag("tag", tag("call", text) | tag("name", text))))

  def parse(input: String) = {
    val r = parseAll(schema, input)
    r match {
      case Success(res, next) => res
      case NoSuccess(err, next) => throw new Exception(""+r)
    }
  }

  class PNode(dt:Parser[XXML]) extends scala.xml.Atom(dt) {
    var parser = dt
    def p = this
    def s = parser.* ^^ {Lst(_)}
  }

  /** A parser that matches a literal string */
  implicit def tag1(s: Node): Parser[XXML] = new Parser[XXML] {
    def apply(in: Input) = {
      s.child.length match {
        case 0 =>
          one(s.label)(in)
        case 1 =>
          val s2 = s.child(0) match {
            case p:PNode => p.parser
          }
          tag(s.label, s2)(in)
        case n =>
          println("n="+n+ " "+s)
          throw new Exception("n="+n+ " "+s)
      }
    }
  }
  implicit def p(p:Parser[XXML]): PNode = new PNode(p)

  implicit def list(parser:Parser[List[XXML]]): PNode = new PNode(parser^^{Lst(_)})


  def html:Parser[XXML] =
    <html>{(
      (<head>{head.*.p}</head> | head.*.p) ~ body ^^
        {case a~b=>Lst(List(a,b))} |
      body
    ).p}</html> |
    (<head>{head.*.p}</head>|head.*.p) ~ body ^^ {case a~b=> Lst(a::List(b))} |
    body

  def head :Parser[XXML] =
    <title>{text.p}</title> |
    <base/> | <bgsound/> | <isindex/> | <nextid/> | <command/> | <link/> | <meta/> |
    <noscript>{inline.*.p}</noscript> |
    script

  def body:Parser[XXML] = <body>{flow.s.p}</body> | flow.s

  def flow:Parser[XXML] =
    <hr/> |
    <article>{flow.*.p}</article> |
    <aside>{flow.*.p}</aside> |
    <blockquote>{flow.*.p}</blockquote> |
    <dfn>{flow.*.p}</dfn> |
    <div>{flow.*.p}</div> |
    <footer>{flow.*.p}</footer> |
    <form>{flow.*.p}</form> |
    <header>{flow.*.p}</header> |
    <nav>{flow.*.p}</nav> |
    <pre>{flow.*.p}</pre> |
    heading | p1 |
    <hgroup>{heading.*.p}</hgroup> |
    <details>{(<summary>inline</summary> ~ flow.* ^^ {case a~b=>Lst(a::b)}).p}</details> |
    <fieldset>{(<legend>inline</legend>.? ~ flow.* ^^ {case a~b=>Lst(a.toList:::b)}).p}</fieldset> |
    <figure>{(flow | <figcaption>{flow.*.p}</figcaption>).*.p}</figure> |
    <ol>{<li>{flow.*.p}</li>.*.p}</ol> |
    <ul>{<li>{flow.*.p}</li>.*.p}</ul> |
    <dl>{(<dt>{flow.*.p}</dt> | <dd>{flow.*.p}</dd>).*.p}</dl> |
    <table>{(
     <caption>{flow.*.p}</caption>.? ~
     <colgroup>{(<col/>|script).*.p}</colgroup>.* ~
     <thead>{tr.*.p}</thead>.? ~
     <tfoot>{tr.*.p}</tfoot>.? ~
     (<tbody>{tr.*.p}</tbody>|tr.*.parser) ~
     <tfoot>{tr.*.p}</tfoot>.? ^^
     {case a~b~c~d~e~f=>a.toList:::b:::c.toList:::d.toList:::e::f.toList}
    ).p}</table> |
    inline

  def heading:Parser[XXML] =
    <h1>{(not(heading) ~> flow).*.p}</h1> |
    <h2>{(not(heading) ~> flow).*.p}</h2> |
    <h3>{(not(heading) ~> flow).*.p}</h3> |
    <h4>{(not(heading) ~> flow).*.p}</h4> |
    <h5>{(not(heading) ~> flow).*.p}</h5> |
    <h6>{(not(heading) ~> flow).*.p}</h6>

  def p1:Parser[XXML] = <p>{p(not(p1) ~> inline.s)}</p>

  def tr:Parser[XXML] = <tr>{(<td>{text.p}</td> | <th>{flow.s.p}</th>).*.p}</tr>
  def inline:Parser[XXML] = text |
    <area/> | <link/> | <meta/> | <br/> | <embed/> |
    <img/> | <input/> | <keygen/> | <wbr/> |
    <abbr>{inline.s.p}</abbr> |
    <address>{inline.s.p}</address> |
    <b>{inline.s.p}</b> |
    <bdi>{inline.s.p}</bdi> |
    <bdo>{inline.s.p}</bdo> |
    <button>{inline.s.p}</button> |
    <canv>{inline.s.p}</canv> |
    <cite>{inline.s.p}</cite> |
    <code>{inline.s.p}</code> |
    <command>{inline.s.p}</command> |
    <data>{inline.s.p}</data> |
    <em>{inline.s.p}</em> |
    <i>{inline.s.p}</i> |
    <iframe>{inline.s.p}</iframe> |
    <kbd>{inline.s.p}</kbd> |
    <label>{inline.s.p}</label> |
    <mark>{inline.s.p}</mark> |
    <noscript>{inline.s.p}</noscript> |
    <output>{inline.s.p}</output> |
    <q>{inline.s.p}</q> |
    <s>{inline.s.p}</s> |
    <samp>{inline.s.p}</samp> |
    <section>{inline.s.p}</section> |
    <small>{inline.s.p}</small> |
    <span>{inline.s.p}</span> |
    <strong>{inline.s.p}</strong> |
    <sub>{inline.s.p}</sub> |
    <sup>{inline.s.p}</sup> |
    <svg>{inline.s.p}</svg> |
    <time>{inline.s.p}</time> |
    <u>{inline.s.p}</u> |
    <var>{inline.s.p}</var> |
    <meter>{inline.s.p}</meter> |
    <progress>{inline.s.p}</progress> |
    <datalist>{(inline | option).s.p}</datalist> |
    <ruby>{(<rt>{inline.s.p}</rt> | <rt>{inline.s.p}</rt>).s.p}</ruby> |
    <a>{flow.s.p}</a> |
    <del>{flow.s.p}</del> |
    <ins>{flow.s.p}</ins> |
    <map>{flow.s.p}</map> |
    <video>{(flow | <source/> | <track/>).s.p}</video> |
    <audio>{(flow | <source/> | <track/>).s.p}</audio> |
    <textarea>{cdata.p}</textarea> |
    <menu>{(<li>{flow.s.p}</li> | flow).s.p}</menu> |
    <object>{(<param/>.* ~ flow.* ^^ {case a~b=>Lst(a:::b)}).p}</object> |
    <select>{(option | <optgroup>{option.s.p}</optgroup>).s.p}</select> |
    script

  def option:Parser[XXML] = <option>{text.p}</option>
  def script:Parser[XXML] =
    <script>{cdata.p}</script> |
    <template>{cdata.p}</template> |
    <style>{cdata.p}</style>

  def parseHTML(input: String) =
    parseAll(html, input) match {
      case Success(res, next) => res
      case NoSuccess(err, next) => throw new Exception(err)
    }

}

object main extends App {
  println(XXMLParser.parseAll(XXMLParser.html,"<table><tr><td>aa<tr/></table>"))
  println(XXMLParser.parseAll(XXMLParser.html,"<body><hr/><table><tr><td>aa<tr/></table></body>"))
  println(XXMLParser.parseAll(XXMLParser.html,"<html><body><hr/><table><tr><td>aa<tr/></table></body></html>"))
  println(XXMLParser.parseAll(XXMLParser.html,"<html><head><title>aaa</title></head><body><hr/><table><tr><td>aa<tr/></table></body></html>"))
}

object main2 extends App {
  def time(any: => Any) {
    for(i <- 0 until 10) {
      val start = System.currentTimeMillis
      any
      println(System.currentTimeMillis - start)
    }
  }
  time {
      for(i <- 0 to 1000)
        XXMLParser.parseHTML("<body><hr/><table><tr><td>aa<tr/></table></body>")
  }

}