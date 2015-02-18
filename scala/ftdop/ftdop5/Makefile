default: run

ftdop5/main.class: ftdop5.scala
	scalac ftdop5.scala

run: ftdop5/main.class

	scala ftdop5.main "@mac(('a ** 'b))a*b; @mac(mul('a,'b))a*b; (@(11111 ** 11111) + @mul(11111, 11111)) / 2"
	scala ftdop5.main "f=fun(a)a+1 f(10)"
	scala ftdop5.main "fib=fun(a)if(a<=1)1 else fib(a-2)+fib(a-1) fib(30)"

clean:
	rm -rf target ftdop5

