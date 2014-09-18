import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import thih._

class PatSpec extends FlatSpec {
  import TIMonad._
  import Pat._
  import Type._
  import Kind._

  it should "tiPat" in {
    runTI { ti =>
      val pat = PWildcard
      val (preds, assumps, ty) = tiPat(ti)(pat)

      preds shouldBe List()
      assumps shouldBe List()
      ty shouldBe TVar(Tyvar("v0", Star))
    }
  }
}
