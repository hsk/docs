import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import thih._

class AssumpSpec extends FlatSpec {

  import Scheme._
  import Type._
  import Kind._
  import Assump._
  import Pred._
  import Subst._

  it should "assump" in {
    val t = TVar(Tyvar("a", Star))
    val assump = Assump("ABC", Forall(List(), Qual(List(), t)))

    assump shouldBe
      Assump("ABC", Forall(List(), Qual(List(), TVar(Tyvar("a", Star)))))
  }

  it should "assumpApply" in {
    val t = TVar(Tyvar("a", Star))
    val assump = Assump("ABC", Forall(List(), Qual(List(), t)))
    val subst: Subst = List((Tyvar("a", Star), tInt))
    val assump2 = assumpApply(subst)(assump)

    assump2 shouldBe
      Assump("ABC", Forall(List(), Qual(List(), TCon(Tycon("Int", Star)))))
  }

  it should "assumpTv" in {
    val t = TVar(Tyvar("a", Star))
    val assump = Assump("ABC", Forall(List(), Qual(List(), t)))
    val tvs = assumpTv(assump)

    tvs shouldBe
      List(Tyvar("a", Star))
  }

  it should "assumpsApply" in {
    val t = TVar(Tyvar("a", Star))
    val assump = Assump("ABC", Forall(List(), Qual(List(), t)))
    val subst = List((Tyvar("a", Star), tInt))
    val assumps = assumpsApply(subst)(List(assump))

    assumps shouldBe
      List(Assump("ABC", Forall(List(), Qual(List(), TCon(Tycon("Int", Star))))))
  }

  it should "assumpsTv" in {
    val t = TVar(Tyvar("a", Star))
    val assump = Assump("ABC", Forall(List(), Qual(List(), t)))
    val tvs = assumpsTv(List(assump))

    tvs shouldBe
      List(Tyvar("a", Star))
  }

  it should "find" in {
    val t = TVar(Tyvar("a", Star))
    val assump = Assump("ABC", Forall(List(), Qual(List(), t)))
    val sc = find("ABC")(List(assump))

    sc shouldBe
      Forall(List(), Qual(List(), TVar(Tyvar("a", Star))))
  }

}