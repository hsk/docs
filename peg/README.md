# PEGの拡張を考える

PEGを拡張して、パターンマッチ構文を作ることを考える。
とりあえず、JavaScriptでパーサコンビネータと、パターンマッチを同じ関数を使って行う。

## a.js

Scalaのunapplyっぽいもの

## b.js

a.matchみたいに書けるようにしてみた

## c.js

this使わずに関数だけで何とかする

## d.js

パーサコンビネータのorを使ってパーシャルファンクションを作る

以下のソースを掃き出す言語を作る。

	var EAdd = data("EAdd")
	var ESub = data("ESub")
	var EMul = data("EMul")
	var EDiv = data("EDiv")
	var EInt = data("EInt")
	var EVar = data("EVar")

	function exp(e) {
	  return p(term,rep(or("+","-"), term)).action(function(a,b){
	    return foldl(a, b, function(a,b){
	      return or(
	        bind(Arr, function(op,b){ if(op!="+") return _; return EAdd(a, b); }),
	        bind(Arr, function(op,b){ if(op!="-") return _; return ESub(a, b); })
	      )(b);
	    });
	  })(e);
	}

	function term(e) {
	  return p(fact,rep(or("*","/"), fact)).action(function(a,b){
	    return foldl(a, b, function(a,b){
	      return or(
	        bind(Arr, function(op,b){ if(op!="*") return _; return EMul(a, b); }),
	        bind(Arr, function(op,b){ if(op!="/") return _; return EDiv(a, b); })
	      )(b);
	    });
	  })(e);
	}

	function fact(e) {
	  return or(
	   p("(", exp, ")").action(function(a,b,c){return b;}),
	   reg(/^[0-9]+/).action(function(a){return EInt(parseInt(a)) })
	  )(e);
	}

	var e = exp("10*20+30*40")
	console.log("e="+e);
	var eval = or(
	  bind(EInt, function(a){ return a; }),
	  bind(EAdd, function(a, b){ return eval(a)+eval(b); }),
	  bind(ESub, function(a, b){ return eval(a)-eval(b); }),
	  bind(EMul, function(a, b){ return eval(a)*eval(b); }),
	  bind(EDiv, function(a, b){ return eval(a)/eval(b); }),
	  function(e){ return e[0]+e.length; }
	)
	console.log("eval="+eval(e[0]));

	console.log(bind(EInt, function(a){return a; })(EInt(10)))

を

	{
		data e = EAdd | ESub | EMul | EDiv | EInt | EVar
	}

	exp = term (("+" | "-") term)* -> {(a,b)=> fold(a,b, {(a,b)=>
		b |> Arr("+",b) => EAdd(a, b) | Arr("-",b) => ESub(a, b)
		
	})}

	term = fact (("*" | "/") fact)* -> {(a,b)=> fold(a,b, {(a,b)=>
		b |> Arr("*",b) => EMul(a, b) | Arr("/",b) => EDiv(a, b)
	})}

	fact =
	   "(" exp ")" -> {(a,b,c)=>b} |
	   reg(/^[0-9]+/) -> {(a)=> EInt(parseInt(a)) }

	{
		e = exp("10*20+30*40")
		console.log("e="+e)
		eval = EInt(a) => a 
		  	| EAdd(a, b) => eval(a)+eval(b)
		  	| ESub(a, b) => eval(a)-eval(b)
		  	| EMul(a, b) => eval(a)*eval(b)
		  	| EDiv(a, b) => eval(a)/eval(b)
		  	| e          => e[0]+e.length
		)
		console.log("eval="+eval(e[0]))
		console.log((EInt(a) => a)(EInt(10)))
	}

から生成するパーサを書く


既に以下のプログラムは出来てる。

	var EBin = data("EBin")
	var EInt = data("EInt")
	var EVar = data("EVar")

	function exp(e) {
	  return p(term,rep(or("+","-"), term)).action(function(a,b){
	    return foldl(a, b, function(a,b){
	      return or(
	        bind(Arr, function(op,b){ return EBin(a, op, b); })
	      )(b);
	    });
	  })(e);
	}

	function term(e) {
	  return p(fact,rep(or("*","/"), fact)).action(function(a,b){
	    return foldl(a, b, function(a,b){
	      return or(
	        bind(Arr, function(op,b){ return EBin(a, op, b); })
	      )(b);
	    });
	  })(e);
	}

	function fact(e) {
	  return or(
	   p("(", exp, ")"),
	   reg(/^[0-9]+/).action(function(a){return EInt(parseInt(a)) })
	  )(e);
	}

これを拡張して行く。まずパーサが必要だからパーサだけ作る。


	var EBin = data("EBin")
	var EInt = data("EInt")
	var EVar = data("EVar")

	function exp(e) {
	  return p(term,rep(or("+","-"), term)).action(function(a,b){
	    return foldl(a, b, function(a,b){
	      return or(
	        bind(Arr, function(op,b){ return EBin(a, op, b); })
	      )(b);
	    });
	  })(e);
	}

	function term(e) {
	  return p(fact,rep(or("*","/"), fact)).action(function(a,b){
	    return foldl(a, b, function(a,b){
	      return or(
	        bind(Arr, function(op,b){ return EBin(a, op, b); })
	      )(b);
	    });
	  })(e);
	}

	function fact(e) {
	  return or(
	   p("(", exp, ")").action(function(a,b,c){ return b; }),
	   reg(/^[0-9]+/).action(function(a){return EInt(parseInt(a)) })
	  )(e);
	}

	function rule(e) {
	  return or(
	  	p("{", act, "}").action(function(a,b,c){ return b; }),
	  	p(reg(/^[a-zA-Z_][a-zA-Z_0-9]*/), "=", exp)
	  )(e);
	}
