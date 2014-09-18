import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import thih._

class LitSpec extends FlatSpec {
  import TIMonad._
  import Lit._

  it should "tiLit" in {
    runTI { ti =>
      val lit = LitInt(123)
      val (preds,ty) = tiLit(ti)(lit)
      printf("preds = %s ty = %s\n", preds, ty)
    }
  }
}
