import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import thih._

class UnifySpec extends FlatSpec {

  import Type._
  import Unify._
  import Kind._

  val t1 = TVar(Tyvar("a", Star))
  val t2 = TVar(Tyvar("b", Star))
  val tv1 = Tyvar("a", Star)
  val t3 = tInt

  it should "mgu" in {
    val subst = mgu(t1)(t2)
    subst shouldBe
      List((Tyvar("a", Star), TVar(Tyvar("b", Star))))

    val subst2 = mgu(t1)(t3)
    subst2 shouldBe
      List((Tyvar("a", Star), tInt))
  }

  it should "varBind" in {
    val subst = varBind(tv1)(t1)
    subst shouldBe List()

    val subst2 = varBind(tv1)(t3)
    subst2 shouldBe
      List((Tyvar("a", Star), tInt))
  }

  it should "match_" in {
    val subst = match_(t1)(t2)
    subst shouldBe
      List((Tyvar("a", Star), TVar(Tyvar("b", Star))))

    val subst2 = match_(t1)(t3)
    subst2 shouldBe
      List((Tyvar("a", Star), tInt))
  }

}
