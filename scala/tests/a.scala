package a

class A[T](a:T)
class B extends A[String]("hoge")
object C extends A[String]("hoge") {
  def p() {
    println("test")
  }
}

object main extends App {
  C.p()
}
