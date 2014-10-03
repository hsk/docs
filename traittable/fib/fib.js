function fib(n) {
  if(n < 2) return 1;
  return fib(n-2)+fib(n-1);
}
function Fib(n) {
  this.n = n;
}
Fib.prototype.fib = function() {
  if (this.n < 2) return 1;
  return new Fib(this.n-2).fib() + new Fib(this.n-1).fib();
}

var start = new Date().getTime();
console.log(fib(40));
console.log(new Date().getTime()-start+"ms");

var start = new Date().getTime();
console.log(new Fib(40).fib());
console.log(new Date().getTime()-start+"ms");
