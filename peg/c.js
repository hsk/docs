// non use this
function _() {
  var o = function () {
    var self = arguments;
    self.match = function() {
      for(var i = 0; i < arguments.length; i++) {
        if(o==arguments[i][0]) {
          var rc = arguments[i][1].apply(this, self);
          if (rc != _) return rc;
        } else if (arguments[i][0] == _) {
          var rc = arguments[i][1].call(this, self);
          if (rc != _) return rc;
        }
      }
      return _;
    }
    return self;
  }
  return o;
}

var EAdd = _()
var EInt = _()
var EVar = _()
function eval(e) {
  return e.match(
    [EInt, function(a){ if(a>10) return _; return a; }],
    [EInt, function(a){ return a * 10; }],
    [EAdd, function(a, b){ // EAdd(EInt(a2),b)
      return a.match([EInt, function(a2){ if (a2 != 1) return _;
        return a2;
      ]);
    }],
    [EAdd, function(a, b){ return eval(a)+eval(b); }],
    [_,    function(e){ return e[0]+e.length; }]
  )
}

console.log(eval(EInt(1)))
console.log(eval(EInt(11)))
console.log(eval(EAdd(EInt(1), EInt(2))))
console.log(eval(EAdd(EInt(2), EInt(2))))
console.log(eval(EVar("aaa")))

