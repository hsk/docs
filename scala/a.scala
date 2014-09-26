package mat

object Main extends App {
  class A(val a:Int)
  object A {
    def unapply(a:A):Option[Int] = {
      println("unapply")
      if (a.a > 0) Some(a.a) else None
    }
  }
  val b = new A(1) match {
    case A(a) => a
    case k:A => -k.a
  }
  val b2 = new A(1) match {
    case A(a) => a
    case k:A => -k.a
  }
  println("b="+b)
  println("b2="+b2)
}
