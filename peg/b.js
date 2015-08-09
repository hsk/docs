function data() {
  function o() {
    this.vars = arguments;
    var self = this;
    this.match = function() {
      for(var i = 0; i < arguments.length; i++) {
        var rc = arguments[i][0].unapply(self, arguments[i][1]);
        if (rc != data) return rc;
      }
      return data;
    }
  }
  o.unapply = function(e, f) {
    if(e instanceof o) return f.apply(this,e.vars);
    return data;
  }
  return o;
}

var EAdd = data()
var EInt = data()

function eval(e) {
  return e.match(
    [EInt, function(a){ if(a>10) return data; return a; }],
    [EInt, function(a){ return a * 10; }],
    [EAdd, function(a, b){ return eval(a)+eval(b); }]
  )
}

console.log(eval(new EInt(1)))
console.log(eval(new EInt(11)))
console.log(eval(new EAdd(new EInt(1), new EInt(2))))

