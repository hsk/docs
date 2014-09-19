import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import thih._

class PatSpec extends FlatSpec {
  import TIMonad._
  import Pat._
  import Type._
  import Kind._
  import Assump._
  import Scheme._
  import Lit._
  import Pred._

  it should "tiPat PVar" in {
    runTI { ti =>
      val pat = PVar("a")
      val (preds, assumps, ty) = tiPat(ti)(pat)

      preds shouldBe List()
      assumps shouldBe
        List(
          Assump("a",Forall(List(),Qual(List(),TVar(Tyvar("v0",Star))))))

      ty shouldBe TVar(Tyvar("v0", Star))
    }
  }

  it should "tiPat PWildcard" in {
    runTI { ti =>
      val pat = PWildcard
      val (preds, assumps, ty) = tiPat(ti)(pat)

      preds shouldBe List()
      assumps shouldBe List()
      ty shouldBe TVar(Tyvar("v0", Star))
    }
  }

  it should "tiPat PAs" in {
    runTI { ti =>
      val pat = PAs("a", PWildcard)
      val (preds, assumps, ty) = tiPat(ti)(pat)

      preds shouldBe List()
      assumps shouldBe
        List(
          Assump("a",Forall(List(),Qual(List(),TVar(Tyvar("v0",Star))))))

      ty shouldBe TVar(Tyvar("v0", Star))
    }
  }

  it should "tiPat PLit" in {
    runTI { ti =>
      val pat = PLit(LitInt(123))
      val (preds, assumps, ty) = tiPat(ti)(pat)

      preds shouldBe
        List(IsIn("Num",TVar(Tyvar("v0",Star))))
      assumps shouldBe List()
      ty shouldBe TVar(Tyvar("v0", Star))
    }
  }

  it should "tiPat PNpk" in {
    runTI { ti =>
      val pat = PNpk("a",10)
      val (preds, assumps, ty) = tiPat(ti)(pat)

      preds shouldBe
        List(IsIn("Integral",TVar(Tyvar("v0",Star))))
      assumps shouldBe
        List(
          Assump("a",Forall(List(),Qual(List(),TVar(Tyvar("v0",Star))))))

      ty shouldBe TVar(Tyvar("v0", Star))
    }
  }

  it should "tiPat PCon" in {
    runTI { ti =>
      val t = TVar(Tyvar("a", Star))
      val assump = Assump("ABC", Forall(List(), Qual(List(), t)))

      val pat = PCon(assump,List())
      val (preds, assumps, ty) = tiPat(ti)(pat)

      preds shouldBe List()
      assumps shouldBe List()
      ty shouldBe TVar(Tyvar("v0", Star))
    }
  }
}
