import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import thih._

class KindSpec extends FlatSpec {

  import Kind._

  it should "kind" in {
    val star = Star
    val kfun1 = Kfun(Star, Star)
    val kfun2 = Kfun(Star, Kfun(Star, Star))
    val kfun3 = Kfun(Star, Kfun(Star, Kfun(Star, Star)))
    val kfun4 = Kfun(Kfun(Star, Star), Kfun(Star, Star))
    val kfun5 = Kfun(Star, Kfun(Kfun(Star, Star), Star))
  }
}
