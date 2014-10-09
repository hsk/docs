package main

import "fmt"
import "time"

func fib(a int) int {
  if a < 2 {
    return 1
  } else {
    return fib(a-2)+fib(a-1)
  }
}

type Fib interface {
  fib() int
}

type Int struct {x int}
func (p *Int) fib() int {
  if p.x < 2 {
    return 1
  } else {
    p1 := Int{p.x - 2}
    p2 := Int{p.x - 1}
    return p1.fib() + p2.fib()
  }
}

type E interface { eval() int }
type EInt struct {x int }
type EAdd struct {x E; y E}
type EMul struct {x E; y E}

func (p *EInt) eval() int { return p.x }
func (p *EAdd) eval() int { return p.x.eval() + p.y.eval() }
func (p *EMul) eval() int { return p.x.eval() * p.y.eval() }
func (p *Int) eval() int { return p.x }
func main() {
  start := time.Now()
  result := fib(40)
  fmt.Printf("fib %d %d %s\n", 40, result, time.Now().Sub(start))

  start = time.Now()
  i:= Int{20}
  i.x = 40
  result = i.fib()
  fmt.Printf("fib %d %d %s\n", i.x, result, time.Now().Sub(start))
  fmt.Printf("eval 40 = %d\n", i.eval())

  i2:= EInt{41}
  fmt.Printf("eval 41 = %d\n", i2.eval())

  add:=EAdd{&EInt{1}, &EInt{22}}
  fmt.Printf("eval 1 + 22 = %d\n", add.eval())

  mul:=EMul{&EAdd{&EInt{1},&EInt{2}}, &EInt{111}}
  fmt.Printf("eval (1+2) * 111= %d\n", mul.eval())

}
