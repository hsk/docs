import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import thih._

class PatSpec extends FlatSpec {
  import TIMonad._
  import Pat._

  it should "test" in {
    runTI { ti =>
      val pat = PWildcard
      val (preds, assumps, ty) = tiPat(ti)(pat)
      printf("tiPat %s %s %s\n", preds, assumps, ty)
    }
  }
}
