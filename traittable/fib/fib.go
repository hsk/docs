package main

import "fmt"
import "time"

func fib(x int) int {
  if x < 2 {
    return 1
  } else {
    return fib(x - 2) + fib(x - 1)
  }
}

type Int struct {x int}
func (p *Int) Fib() int {
  if p.x < 2 {
    return 1
  } else {
    return (&Int{p.x-2}).Fib() + (&Int{p.x-1}).Fib()
  }
}

type E interface { eval() int }
type EInt struct { x int }
type EAdd struct { x E; y E }
type ESub struct { x E; y E }
type EMul struct { x E; y E }
type EDiv struct { x E; y E }
func (e *EInt) eval() int { return e.x }
func (e *EAdd) eval() int { return e.x.eval() + e.y.eval() }
func (e *ESub) eval() int { return e.x.eval() - e.y.eval() }
func (e *EMul) eval() int { return e.x.eval() * e.y.eval() }
func (e *EDiv) eval() int { return e.x.eval() / e.y.eval() }

func main() {

  start := time.Now()
  a := Int{40}
  fmt.Println(a.Fib())
  fmt.Println(time.Now().Sub(start))

  start = time.Now()
  fmt.Println(fib(40))
  fmt.Println(time.Now().Sub(start))

  add := &EMul{&EInt{10},&EAdd{&EInt{1},&EInt{2}}}
  fmt.Println(add.eval())
}

