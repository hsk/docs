import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import thih._

class LitSpec extends FlatSpec {
  import TIMonad._
  import Lit._
  import Type._
  import Kind._
  import Pred._

  it should "tiLit" in {
    runTI { ti =>
      val lit = LitInt(123)
      val (preds, ty) = tiLit(ti)(lit)

      preds shouldBe
        List(IsIn("Num", TVar(Tyvar("v0", Star))))

      ty shouldBe
        TVar(Tyvar("v0", Star))
    }
  }
}
