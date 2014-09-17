import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import thih._

class UnifySpec extends FlatSpec {

  import Type._
  import Unify._

  val t1 = TVar(Tyvar("a", Kind.Star))
  val t2 = TVar(Tyvar("b", Kind.Star))
  val tv1 = Tyvar("a", Kind.Star)

  it should "mgu" in {
    val subst = mgu(t1)(t2)
    subst shouldBe
    List((Tyvar("a",Kind.Star),TVar(Tyvar("b",Kind.Star))))
  }

  it should "varBind" in {
    val subst = varBind(tv1)(t1)
    subst shouldBe List()
  }

  it should "match_" in {
    val subst = match_(t1)(t2)
    subst shouldBe
    List((Tyvar("a",Kind.Star),TVar(Tyvar("b",Kind.Star))))
  }

}
