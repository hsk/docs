<?php

function fib($n) {
  if ($n < 2) return 1;
  return fib($n-2)+fib($n-1);
}

class Fib {
  function __construct($n) {
    $this->n = $n;
  }
 
  function fib() {
    if ($this->n < 2) return 1;
    return new Fib($this->n-2).fib() + new Fib($this->n-1).fib();
  }
}

function tim() {
	return (int)(microtime(true) * 1000);
}

$start = tim();
printf("%d\n", fib(40));
printf("%dms\n",tim()-$start);

$start = tim();
printf("%d\n", new Fib(40).fib());
printf("%dms\n",tim()-$start);
