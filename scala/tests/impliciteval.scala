package impliciteval

object main extends App {
  trait E
  case class EInt(n:Int) extends E
  case class EVar(x:String) extends E
  case class EAdd(e1:E,e2:E) extends E
  case class ELet(x:String,e1:E,e2:E) extends E

  def eval(e:E)(implicit env:Map[String,Int]):Int = {
    e match {
    case EInt(n) => n
    case EVar(s) => env(s)
    case EAdd(e1,e2) => eval(e1)+eval(e2)
    case ELet(x,e1,e2) => eval(e2)(env + (x -> eval(e1)))
    }
  }

  println(eval(ELet("a",EInt(1),ELet("b",EInt(2), EAdd(EVar("a"),EVar("b")))))(Map()))

}
