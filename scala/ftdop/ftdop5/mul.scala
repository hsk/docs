package mul

import ftdop5._

object plugin {
  fun("mul"){case((a:Int,",",b:Int),_) =>
      a * b
  }
  mac(("test","(",Symbol("a"),")")){ case (e:Env) =>
      e("a").asInstanceOf[Int] * 100
  }
}
