function EInt(a) {
  this.a = a;
}
EInt.unapply = function(e, f) {
  if(e instanceof EInt) return f(e.a);
}
function EAdd(a,b) {
  this.a = a;
  this.b = b;
}
EAdd.unapply = function(e, f){
  if(e instanceof EAdd) return f(e.a,e.b);
}

function match(e) {
  for(var i = 1; i < arguments.length; i++) {
    var rc = arguments[i][0].unapply(e, arguments[i][1]);
    if (rc != null) return rc;
  }
}

var e = new EAdd(new EInt(1),new EInt(2));
console.log(eval(new EInt(1)))
console.log(eval(e))

function eval(e) {
  return match(
    e,
    [EAdd, function(a,b){ return eval(a)+eval(b); }],
    [EInt, function(a){ return a; }]
  )
}

console.log(eval(e))

