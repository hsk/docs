default: run

ftdop4/main.class: ftdop4.scala
	scalac ftdop4.scala

run: ftdop4/main.class

	scala ftdop4.main "mac(mul('a,'b))a*b mul(1111,1111)"
	scala ftdop4.main "f=fun(a)a+1 f(10)"
	scala ftdop4.main "fib=fun(a)if(a<=1)1 else fib(a-2)+fib(a-1) fib(30)"

clean:
	rm -rf target ftdop4

