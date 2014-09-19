import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import thih._

class TIMonadSpec extends FlatSpec {

  import Kind._
  import Type._
  import Subst._
  import Pred._
  import Scheme._
  import List._
  import TIMonad._

  it should "ti" in {
    val subst = List((Tyvar("a", Star), tInt))
    val ti = Ti(subst, 1)

    ti shouldBe
      Ti(List((Tyvar("a", Star), TCon(Tycon("Int", Star)))), 1)
  }

  it should "runTI" in {
    val n = runTI { ti =>
      ti.n += 1
      ti.n
    }

    n shouldBe 1
  }

  it should "getSubst" in {
    runTI { ti =>
      val subst = getSubst(ti)

      subst shouldBe List()
    }
  }

  it should "extSubst" in {
    runTI { ti =>
      val subst = List((Tyvar("a", Star), tInt))
      extSubst(ti)(subst)
      val subst2 = getSubst(ti)

      subst2 shouldBe List((Tyvar("a", Star), TCon(Tycon("Int", Star))))
    }
  }

  it should "unify" in {
    runTI { ti =>
      val t1 = TVar(Tyvar("a", Star))
      unify(ti)(t1)(tInt)

      t1 shouldBe TVar(Tyvar("a", Star))
    }
  }

  it should "newTVar" in {
    runTI { ti =>
      val t1 = newTVar(ti)(Star)
      t1 shouldBe TVar(Tyvar("v0", Star))

      unify(ti)(t1)(tInt)

      t1 shouldBe TVar(Tyvar("v0", Star))

      val t2:Type_ = typeApply(getSubst(ti))(t1)
      t2 shouldBe tInt
    }

  }

  it should "freshInst" in {
    runTI { ti =>
      val ty = TVar(Tyvar("a", Star))
      val sc = toScheme(ty)

      val tq:Qual[Type_] = freshInst(ti)(sc)

      sc shouldBe Forall(List(), Qual(List(), TVar(Tyvar("a", Star))))
      tq shouldBe Qual(List(), TVar(Tyvar("a", Star)))
    }
  }
}