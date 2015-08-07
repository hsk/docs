# 拡張拡張可能マークアップ言語 XXML

## 1. はじめに

HTMLはWebブラウザで用いられ非常に人気がある言語です。
HTMLはSGMLという言語を元に作られていましたが、SGMLの仕様は複雑でした。
そこで単純化されたXMLが作られました。
XMLもなかなかの人気で様々な言語が作成されました。
HTMLはXHTMLに全て置き換わるかと予想されていました。
しかし、XMLは記述が長くなってしまう問題がありました。
XHTMLはHTMLを置き換える事はなく、XMLの変わりにYAMLやJSONが用いられています。

しかし、HTMLは未だに用いられています。
HTMLは好きだけど、XMLは嫌いという人が多いのです。

ここで、XMLは単純化しすぎたのではないか？という疑問が生じたわけです。
そこで、XMLを拡張し、より便利なXMLを作成する事を考えました。


## 2. XMLの拡張アイディア

ここでは、どのようにXMLを拡張したら便利かを考えます。
タグの拡張とアトリビュートの拡張という２つの観点で拡張するポイントを洗い出します。

## 2.1. タグ拡張

### 2.1.1. シングルタグの"/"を省略出来る

XMLでは、閉じないタグには必ず/をつけなくてはなりません。
HTMLでは以下のように\<br\> タグを/なしに書く事が出来ました。

    <br>

しかしXMLでは

    <br/>

と書かなくてはなりません。これは言語としては不便です。C言語の;は美しくないので書かなくて済むなら書きたくない。
というような言語は多く、JavaScriptやActionScript,Scalaなどがそのようになっています。
/を書かなくても済むなら書かないほうが楽です。

### 2.1.2. タグ内テキスト取得タグ

HTMLのscript、style、textareaタグは内部のテキストはタグとして認識されません。
昔のHTMLでは以下のようなHTMLのコメントの中にjavascriptを書いていた時代がありました。

    <script type="javascript">
    <!--//
      alert("test")
    //-->
    </script>

XML時代には以下のように書くようになるのかと予想していました。

    <script type="javascript">
    <[CDATA[
      alert("test")
    ]]>
    </script>

しかしHTMLのscriptタグにCDATAセクションを一般に使われることはありませんでした。

タグ名を登録すると、タグ内を只のテキストとして認識出来る出来るようにすると便利でしょう。

### 2.1.3. ブロックタグの閉じタグ省略

    <html>
      <body>
        <table>
          <tr>
            <td>a
            <td>b
          <tr>
            <td>c
            <td>e
        </table>
        <table>
          <tr>
             <td>a
             <td>b
          <tr>
             <td>e
             <td>f


というHTMLはブラウザで正しく表示されました。この閉じタグの省略機能は非常に便利です。
この機能をあっさりと実現出来たら良いでしょう。
html,body,table,tr,tdタグはブロックタグであると登録しておけば、正しくパース出来る言語が作れたら便利です。


## 2.2. アトリビュート拡張

### 2.2.1. アトリビュート省略

HTMLのcheckboxはアトリビュートにcheckとだけ書けばよかったのですが、xmlでは必ず値を書かなくてはなりませんでした。
値を省略出来ればよいでしょう。

### 2.2.2. アトリビュートの、ダブルクォート、クォート省略

ダブルクォートは省略出来て良いですよね。

### 2.2.3. アトリビュート名省略

doctypeにアトリビュート名はありません。名前があるべき箇所に文字列が現れた場合は、名前が空のアトリビュートとして扱うと便利でしょう。

### 2.2.4. アトリビュート内タグ

アイディアとしては、属性値が文字列だけでは不便なのでタグを書きたい。

    <a href=<a>hoge</a>>aa</a>

更に、アトリビュート名の省略等も考えると以下のように書けても良いかもしれない。

    <table<tr<td"text"/>/><tr<td"text"/>/>/>

この拡張は名前付きの関数呼び出しと、名前なしの関数呼び出しに対応する。
通常のタグ内のデータはRubyのブロックに相当する。

### 2.3. 文法定義

2.1.3.のタグの省略では限定的で、タグの省略を本格的に行いたいのであれば、文法定義が必要でした。
ここでは、文法定義をXMLで行い、その文法定義を用いてパースが可能となるパーサを考えます。

#### パーサコンビネータでtable,tr,tdを実装する

Scalaのパーサコンビネータは、パーサの関数を合成する事でテーブルを定義する事が出来ます。

#### 動的にパーサを組み合わせる

パーサコンビネータは関数を組み合わせる事でパーサを作りますが、その合成はネイティブなScalaの関数でなくてはなりません。これを動的なデータを元に合成する事が可能にする事を考えます。


    def exps = rep1(exp)
    def exp = table | schema | text
    def table = tag("table", tr) 
    def tr = tag("tr", td)
    def td:Parser[XXML] = tag("td", exp)

このexpsを動的なデータから構成出来ればよいわけです。
Scalaで書くと以下のデータ構造があればよいでしょう。
    
    trait Se
    case class Rep1(a:Se) extends Se
    case class Or(ses:List[Se]) extends Se
    case class Tag(s:String, se:Se) extends Se
    case class Var(s: String) extends Se
    case class Schema(start:String, rules:Map[String, Se])

#### SchemaからParser[XXML]へコンパイル

データは定義したので、Schemaからパーサを作成します。

#### XXMLからSchemaへコンパイル

SchemaをXXMLからコンパイルします。

#### Stringから直接Schemaをパースする

#### xxmlschemaの最適化

とりあえず作った文法だったので、より良い文法を考えると良いでしょう。

## 3. 実装

## 3.1. タグの実装

### 3.1.1. シングルタグの"/"を省略出来るXMLパーサ拡張実装

閉じタグがなくても大丈夫なパーサを作ってみましょう。

    
    package tes
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
      def blocks = name //.withFilter{_=="p"}
      def exp:Parser[XXML] = (comment ^^ {Comment(_)}).
              | ((("<" ~> blocks <~ ">") ~ exps ~ ("<" ~> "/" ~> blocks <~ ">")).withFilter {case a~b~c =>a==c} ^^
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

このようなパーサコンビネータで実現可能です。速度は遅いかもしれませんが、コンバータを作ればよいでしょう。

### 3.1.2. テキストを含むタグ

    textTagsの集合に含まれるタグは、タグを無視したテキストを含婿とが出来るように拡張しました。

    package xxml1_2
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

      var tags:List[String] = List()
      def tagText = ("<" ~> name <~ ">").filter{
        case n => if (textTags.contains(n)) { tags = n :: tags; true } else false
      } ~ rep("[^<]+".r | not("<" ~> "/" ~> tags.head) ~> "<") ~
      opt("<"~>"/"~>name<~">") ^^
      { case n~t~n2=> Block(n,List(Text(t.foldLeft(""){_+_})))}

      def exp:Parser[XXML] = (comment ^^ {Comment(_)}).
              | (tagText).
              | ((("<" ~> name <~ ">") ~ exps ~ ("<" ~> "/" ~> name <~ ">")).filter {case a~b~c =>a==c} ^^
                { case a~b~c => Block(a, b) } ).
              | ("<" ~> name <~ opt("/") <~ ">" ^^ { One(_) }).
              | (text ^^ {Text(_)}) 

      def exps = rep(exp)


      def parse(input: String) = parseAll(exps, input)
      var textTags = Set("script","textarea","style")
    }

    object main extends App {
      val res = XXMLParser.parse("<!--comme--nt--><p>ab<br> cd</p><script><p>test</p></script>")
      println("res="+res)
    }

比較的簡単に実装出来ました。

### 3.1.3. タグ省略

タグがネストするという制限で実装してみました。

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

### 3.2.1. 属性のクォート省略

比較的楽に実装出来ます。

    package xxml2_1
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
      def attr = name ~ opt("=" ~> "\"([^\"]|\\.)+\"".r) ^^ {
        case a~Some(b) => (a,b.substring(1,b.length-1))
        case a~None => (a,a)
      }
      def attrs = rep(attr) ^^ {l => l.foldLeft(Map[String,String]()){case(a,(b,c))=>a + (b->c)}}
      def exp:Parser[XXML] = (comment ^^ {Comment(_)}).
              | ((("<" ~> name ~ attrs <~ ">") ~ exps ~ ("<" ~> "/" ~> name <~ ">")).filter {case a~at~b~c =>a==c} ^^
                { case a~at~b~c => Block(a, at, b) } ).
              | ("<" ~> (name ~ attrs) <~ opt("/") <~ ">" ^^ { case a~b=>One(a,b) }).
              | (text ^^ {Text(_)}) 

      def exps = rep(exp)


      def parse(input: String) = parseAll(exps, input)
    }

    object main extends App {
      val res = XXMLParser.parse("<p a=\"test\" checked>ab<br> cd</p>")
      println("res="+res)
    }

### 3.2.2 属性値のクォート省略

簡単に実装出来ました。

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

### 3.2.3. 属性の名前省略

簡単に実装出来ます。

    package xxml2_3
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

      } | "\"([^\"]|\\.)+\"".r ^^ {b=>("",b.substring(1,b.length-1))}

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
      val res = XXMLParser.parse("<p \"aaa\" a=\"test\" checked b=abc>ab<br> cd</p>")
      println("res="+res)
    }

### 3.3. 文法定義

#### 3.3.1. 文法定義とパース

パーサコンビネータを使って、文法定義する事を考えましょう。
何度もタグを使って定義するのは面倒なので、tag関数を作り、タグ名とパーサを渡すとパーサが出来ると楽でしょう。

あとは、tag関数を使ってtableパーサとtrパーサとtdパーサを作って呼び出すだけです。
このパーサは、単純化して考えるため属性はありません。

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

#### 3.3.2. XXMLSchema

スキーマとなるデータ定義(Se)を作り、Seを元にパーサを構成するcompile関数を作ります。
また、xxmlからSeを生成する関数を作ります。
あとは組み合わせれば、XXMLで文法を定義して、タグを省略出来るようになります。

    package xxml3_2
    import util.parsing.combinator._

    trait XXML
    case class Block(name:String, ls:List[XXML]) extends  XXML
    case class Comment(s:String) extends XXML
    case class One(name:String) extends XXML
    case class Text(s:String) extends XXML
    case class Lst(xs:List[XXML]) extends XXML

    trait Se
    case class Rep1(a:Se) extends Se
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

      def exps = rep1(exp)
      def exp = schema | text

      def schema =
        tag("schema",
          tag("start", text) |
          tag("rule",
            tag("name", text) |
            tag("call", text) |
            tag("rep1", text) |
            tag("tag", tag("call", text) | tag("name", text))))

      def compile(schema:Schema): Parser[XXML] = {
        var env = scala.collection.mutable.Map[String, Parser[XXML]]("text"->text)

        def get(s: String): Parser[XXML] = new Parser[XXML] {
          def apply(in: Input): ParseResult[XXML] = {

            val parser = env(s)
            parser.apply(in)
          }
        }

        def comp(se:Se):Parser[XXML] = {
          se match {
            case Rep1(se) => rep1(comp(se))^^{Lst(_)}
            case Or(s::ss) => ss.foldLeft(comp(s)){(a,b)=>a|comp(b)}
            case Tag(s,se) => tag(s,comp(se))
            case Var(s) =>
              if (env.contains(s)) env(s)
              else
                get(s)
          }
        }
        schema.rules.foreach{
          case(name,se)=>
            env += (name -> comp(se))
        }
        env(schema.start)
      }
      def compile(xxml:XXML):Schema = {
        def schema(xxml:XXML):Schema =
          xxml match {
            case Block("schema",Block("start",List(Text(start)))::ls) =>
              Schema(start, rules(ls))
          }
        def rules(ls:List[XXML]):Map[String,Se] = {
          var env = Map[String,Se]()
          ls.foldLeft(env) {
            case (env,Block("rule", Block("name",List(Text(name)))::ls)) =>
              env + (name -> Or(ors(ls)))
          }
        }
        def ors(ls:List[XXML]):List[Se] = {
          ls.map {
            case Block("call",List(Text(s))) => Var(s)
            case Block("rep1",List(Text(s))) => Rep1(Var(s))
            case Block("tag", Block("name",List(Text(name)))::ls) => Tag(name, Or(ors(ls)))
          }
        }
        schema(xxml)
      }

      def parse(input: String) = {
        parseAll(exps, input).get
      }


    }

    object main extends App {
      val res = XXMLParser.parse("""
    <schema>
      <start>exps</>
      <rule>
        <name>exps</>
        <rep1>exp</>
      <rule>
        <name>exp</>
        <call>table</>
        <call>text</>
      <rule>
        <name>table</>
        <tag>
          <name>table</>
          <call>tr</>
        </>
      <rule>
        <name>tr</>
        <tag>
          <name>tr</>
          <call>td</>
        </>
      <rule>
        <name>td</>
        <tag>
          <name>td</>
          <call>exp</>
        </>
    </schema>
    """)

      val res2 = XXMLParser.compile(res.head)
      val res3 = XXMLParser.compile(res2)
      println(XXMLParser.parseAll(res3, """
    <table>
      <tr>
        <td>aaa
        <td>aaa
      <tr>
        <td>aaa
        <td>aaa
    </>
    <table>
      <tr>
        <td>aaa
    """).get)
    }


## 4. 検討

３章では拡張のアイディアを実際に実装しました。
この章では、速度の面や、使い勝手、実装の複雑さなどを検証します。

ほとんどのアイディアについてパーサコンビネータを使って実装をする事が出来ました。
タグ省略以外は実用的であると言っても良いでしょう。
タグを省略出来るようにするには、より高度な文法チェックが欲しい所です。

タグの省略は残念ながら以下の書き方をうまく処理出来ません。

    <table>
      <tr>
        <td>a
        <td>b
      <tr>
        <td>a
        <td>b

以下のように処理されてしまいます。

    <table>
      <tr>
        <td>a
        <td>b
          <tr>
            <td>a
            <td>b

以下のように書かなくてはなりません。

    <table>
      <tr>
        <td>a
        <td>b
      </tr>
      <tr>
        <td>a
        <td>b


基本的に自分にしか再入を許さないだけなのでドンドン再入してしまいます。
スタックを用いて、親にいたら抜けるという実装も可能でしょう。

    <table>
      <tr>
        <td>a
        <td>
          <table>
            <tr>
              <td>a
      <tr>
        <td>a
        <td>b

その場合、以上のようなネストに対応出来ません。難しい所です。

１ラインタグの/は省略可能、テキストタグが登録出来て、属性の値、属性のクォート、属性名の省略が可能なように拡張したXMLは実装が楽でかつ便利であると言えるでしょう。

3.1と、3.2では文法定義をする事が出来るようにする事で、タグの省略ができるようにしました。
属性や単一タグの定義はありませんが、ここから拡張すれば、現実的に使えるパーサが作れるでしょう。

今回検討したパーサの実装を以下に示します:

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

Schemaは以下のように実装出来ます:

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

使い方の例は以下の通りです:

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

## 4. まとめ

１章で、現在のXMLの問題を提起し、２章で問題の解決方法を考え、３章で実装しました。
４章では、仕様と実装についての検討を行いました。
SGMLについて詳しく調べずに、XMLの拡張を考えあらたにXMLを拡張してみました。

１ラインタグの/は省略可能、テキストタグが登録出来て、属性の値、属性のクォート、属性名の省略が可能なように拡張したXXMLの実装をシンプルに行いました。

    <!-- comment -->
    <p "aaa" a="test" b=abc checked>
      ab<br>
      ab<br/>
      <script>
        hoge < 1
      </script>
    </p>

以上のような記述が可能なXMLパーサを構築しました。

またXMLSchemeのような文法定義を行う事で、タグを省略可能とするパーサを作成しました。

    <!RULES es>
    <! es>
      <!l e>
    <! e>
      <tb>
      <text>
    <! tb>
      <!t table><tr>
    <! tr>
      <!t tr><td>
      <!t th><td>
    <! td>
      <!t td><e>

<!RULES>タグは、文法の開始と開始ルール名を表します。
<!>タグはルール名を表します。
<!l>タグはループを表します。
<!t>タグはタグの定義を表します。
RULES自体も、終了タグの省略が可能なので以上の定義が可能です。

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

のようにタグを省略して書く事が出来ます。

省略したタグを全て書き出すと以下のような意味となります。

    <!RULES es="es">
      <! es="es">
        <!l e="e"/>
      </!>
      <! e="e">
        <tb/>
        <text/>
      </!>
      <! tb="tb">
        <!t table="table"><tr/></!t>
      </!>
      <! tr="tr">
        <!t tr="tr"><td/></!t>
        <!t th="th"><td/></!t>
      </!>
      <! td>
        <!t td="td"><e/></!t>
      </!>
    </!RULES>

    <table>
      <th>
        <td>aaa</td>
        <td>aaa</td>
      </th>
      <tr>
        <td>aaa</td>
        <td>aaa</td>
      </tr>
    </table>
    <table>
      <tr>
        <td>aaa</td>
      </tr>
    </table>


## 5. 今後の研究

パーサコンビネータによる実装は必ずしも効率的ではありません。手を抜いているにもかかわらず、nu.validator.htmlparserと比較して50倍ほど性能の差があります。
より効率的なパーサの検討が必要でしょう。
エスケープシーケンスについての処理は行いませんでしたのでエスケープシーケンスの処理も追加すると良いでしょう。

しかしながら、HTML5の補正機能は様々なモードがありアドホックな処理が満載です。[\[1\]](#a1)
HTML５と同じようなパーサを気軽に作る事は無理でしょう。XMLよりはHTML5に近い言語をつくり、その補正機能が十分でかつ高速なら使って良いと判断出来るかもしれません。


### tes.scala

scalaのxmlリテラルでネイティブに定義してみたもの

### tes2.scala

xmlは使わず、scalaのみでHTMLに似た言語を定義したもの。(タグ省略時の動作が違う)

### tes3.scala

htmlに合わせた、タグの補正を出来るようにしてみているもの。
invalidなタグをどう扱うかが難しいので、不完全だ。

## 類似の研究

- HaXML
     - https://hackage.haskell.org/package/HaXml-1.24.1/docs/src/Text-XML-HaXml-Html-Parse.html


## HTML5 パーサ

- https://en.wikipedia.org/wiki/Comparison_of_HTML_parsers
- https://github.com/jhy/jsoup

- <a name="a1"> [1] http://www.w3.org/TR/html5/syntax.html#parsing-main-intable

