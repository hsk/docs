package plugin_test
import scala.collection.mutable.Map
object main extends App {

  var env = Map[Any,Any]()

  def plugin(a:String)(f:Function[Any,Any]) {
    env += (a -> f)
  }

  plugin("load") {case Symbol(name) =>
      println("load "+name)
      val systems = this.getClass().getClassLoader()
      systems.loadClass(name+".plugin$").getField("MODULE$").get(null)
  }


  def eval(e:Any,env:Map[Any,Any]):Any = {
    e match {
    case (a:String) => env(a)
    case (a,"(",b,")") =>
      var f = eval(a,env)
      var b2 = eval(b,env)
      f match {
        case f:Function1[_,_] => f.asInstanceOf[Function1[Any,Any]](b2)
      }
    case (a,",",b) =>
      var a1 = eval(a,env)
      var b1 = eval(b,env)
      (a1,",",b1)
    case (a,"@",b) =>
      eval(a,env)
      eval(b,env)
    case a:Symbol => a
    case (a:Int) => a
    }
  }

  println(eval((("load","(",Symbol("plugin_test"), ")"),"@",("mul","(",(1,",",2),")"))  ,env))

}

object plugin {
  main.plugin("mul"){case(a:Int,",",b:Int) =>
      a * b
  }
}