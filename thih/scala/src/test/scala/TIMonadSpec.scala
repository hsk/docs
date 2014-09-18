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
    printf("ti = %s\n", ti)
  }

  it should "runTI" in {
    val n = runTI {ti =>
      ti.n += 1
      ti.n
    }
    printf("n = %d\n", n)
  }

  it should "getSubst" in {
    runTI { ti =>
      val subst = getSubst(ti)
      printf("subst=%s\n", subst)
    }
  }
  it should "extSubst" in {
    runTI { ti =>
      val subst = List((Tyvar("a", Star), tInt))
      extSubst(ti)(subst)
      val subst2 = getSubst(ti)
      printf("subst2=%s\n", subst2)
    }
  }

  it should "unify" in {
    runTI { ti =>
      val t1 = TVar(Tyvar("a", Star))
      unify(ti)(t1)(tInt)
      printf("t1=%s\n", t1)
    }
  }
  it should "newTVar" in {
    runTI { ti =>
      val t1 = newTVar(ti)(Star)
      unify(ti)(t1)(tInt)
      printf("t1=%s\n", t1)
    }

  }
  it should "freshInst" in {
    runTI { ti =>
      val ty = TVar(Tyvar("a", Star))
      val sc = toScheme(ty)
      printf("scheme = %s\n", sc)
      val tq = freshInst(ti)(sc)
      printf("freshInst = %s\n", tq)
    }
  }
}