import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import thih._

class SchemeSpec extends FlatSpec {

  import Type._
  import Kind._
  import Scheme._
  import Pred._

  it should "scheme" in {

    val ty = TVar(Tyvar("a", Star))
    val pred = IsIn("Num", ty)
    val sc = Forall(List(), Qual(List(pred), ty))

    sc shouldBe
      Forall(List(), Qual(List(IsIn("Num", TVar(Tyvar("a", Star)))), TVar(Tyvar("a", Star))))

  }

  it should "schemeApply" in {
    val ty = TVar(Tyvar("a", Star))
    val pred = IsIn("Num", ty)
    val sc = Forall(List(), Qual(List(pred), ty))
    val subst = List((Tyvar("a", Star), tInt))
    val sc1 = schemeApply(subst)(sc)

    sc1 shouldBe
      Forall(List(), Qual(List(IsIn("Num", TCon(Tycon("Int", Star)))), TCon(Tycon("Int", Star))))

  }

  it should "schemeTv" in {
    val ty = TVar(Tyvar("a", Star))
    val pred = IsIn("Num", ty)
    val sc = Forall(List(), Qual(List(pred), ty))
    val tvs = schemeTv(sc)

    tvs shouldBe
      List(Tyvar("a", Star))

  }

  it should "quantify" in {
    val tyvar = Tyvar("a", Star)
    val ty = TVar(Tyvar("a", Star))
    val pred = IsIn("Num", ty)
    val qual = Qual(List(pred), fn(ty)(tInt))
    val sc = quantify(List(tyvar))(qual)

    sc shouldBe
      Forall(List(Star),
        Qual(List(IsIn("Num", TGen(0))),
          TAp(TAp(TCon(Tycon("(=>)", Kfun(Star, Kfun(Star, Star)))), TGen(0)), TCon(Tycon("Int", Star)))))
  }

  it should "toScheme" in {
    val ty = TVar(Tyvar("a", Star))
    val sc = toScheme(ty)

    sc shouldBe
      Forall(List(), Qual(List(), TVar(Tyvar("a", Star))))

  }
}