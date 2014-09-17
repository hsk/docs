import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import thih._

class KindSpec extends FlatSpec {
  import thih.Kind._
  it should "test" in {
    val star = Kind.Star
    val kfun1 = Kind.Kfun(Kind.Star, Kind.Star)
    val kfun2 = Kind.Kfun(Kind.Star, Kind.Kfun(Kind.Star, Kind.Star))
    val kfun3 = Kind.Kfun(Kind.Star, Kind.Kfun(Kind.Star, Kind.Kfun(Kind.Star, Kind.Star)))
    val kfun4 = Kind.Kfun(Kind.Kfun(Kind.Star, Star), Kind.Kfun(Kind.Star, Star))
    val kfun5 = Kind.Kfun(Kind.Star,Kind.Kfun(Kind.Kfun(Kind.Star, Star), Star))
  }
}
