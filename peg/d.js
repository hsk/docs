function _() {
  return function () {
    return arguments;
  }
}

function startsWith(i, param) {
  var ilen = i.length;
  var len = param.length;
  if (len > ilen) return false;
  else return i.substring(0, len) == param;
}

function nreg(param) {
  return function(i) {
    var m = i.match(param);
    if (m === null) return _;
    return [m[0], i.substring(m[0].length)];
  };
}

function nstr (param) {
  return function(i) {
    if(!startsWith (i, param)) return _;
    return [param, i.substring(param.length)];
  };
}

var skip = nreg(/^(\s|\(\*[\s\S]*\*\))*/);
var str = function(s) { return skip.next(nstr(s));};
var reg = function(s) { return skip.next(nreg(s));};
var Arr = data("Arr");
function or() {
  var args = arguments;
  for(var i = 0; i < args.length;i++)
    if(typeof(args[i])=="string") args[i]=str(args[i]);
  return function(e){
    for(var i = 0; i < args.length; i++) {
      var rc = args[i](e);
      if(rc != _) return rc;
    }
    return _;
  };
}

function p() {
  var args = arguments;
  for(var i = 0; i < args.length;i++)
    if(typeof(args[i])=="string") args[i]=str(args[i]);
  return function(s) {
    var rs = Arr();
    for(var i = 0; i < args.length; i++) {
      var r = args[i](s);
      if(r === _) return _;
      rs.push(r[0]);
      s = r[1];
    }
    return [rs,s];
  };
}
Function.prototype.next=function(that) {
  var thiz = p(this,that);
  return function(i) {
    var r = thiz(i); if(r === _) return r;
    return [r[0][1], r[1]];
  };
};

Function.prototype.prev=function(that) {
  var thiz = p(this,that);
  return function(i) {
    var r = thiz(i); if(r === _) return r;
    return [r[0][0], r[1]];
  };
};

Function.prototype.action=function(f) {
  var thiz = this;
  return function(i) {
    var r = thiz(i); if(r === _) return r;
    if(!(r[0] instanceof Array))r[0] = [r[0]];
    return [f.apply(this, r[0]), r[1]];
  };
};

function opt () {
  var thiz = p.apply(this, arguments);
  return function(i) {
    var r = thiz(i); if(r === _) return [_, i];
    return r;
  };
}

function rep () {
  var thiz = p.apply(this, arguments);
  return function (i) {
    var rs = Arr();
    while(true){
      var r = thiz(i); if (r === _) return [rs, i];
      rs.push(r[0]);
      i = r[1];
    }
  };
}

function rep1 () {
  return rep.apply(this, arguments).action(function(){
    if(arguments.length < 1) return _;
    return arguments;
  });
}

function not () {
  var thiz = p.apply(this, arguments);
  return function(i) {
    var r = thiz(i); if(r === _) return ["", i];
    return _;
  };
}

function bind(o,f) {
  return function(e) {
    if(!("tag" in e) || e.tag != o) return _;
    return f.apply(this, e);
  }
}

function data(name) {
  var o = function() {
    var self = [].slice.apply(arguments);
    self.tag = o;
    self.toString = function() {
      return name + "(" + [].slice.apply(self).join(",")+")";
    };
    return self;
  }
  return o;
}

function map(a, f) {
  var b = Arr();
  for(var i = 0; i < a.length; i++) b[i] = f(a[i]);
  return b;
}

function reduce(a, f) {
  var b = a[0];
  for(var i = 1; i < a.length; i++) b = f(b, a[i]);
  return b;
}
function foldl(v, a, f) {
  for(var i = 0; i < a.length; i++) v = f(v, a[i]);
  return v;
}


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
   p("(", exp, ")"),
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


