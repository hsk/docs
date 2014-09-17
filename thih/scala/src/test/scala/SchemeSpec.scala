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
    val sc = Forall(List(),Qual(List(pred),ty))
    printf("scheme = %s\n", sc)
  }

  it should "schemeApply" in {
    val ty = TVar(Tyvar("a", Star))
    val pred = IsIn("Num", ty)
    val sc = Forall(List(),Qual(List(pred),ty))
    val subst = List((Tyvar("a", Star), tInt))
    val sc1 = schemeApply(subst)(sc)
    printf("scheme = %s\n", sc1)

  }

  it should "schemeTv" in {
    val ty = TVar(Tyvar("a", Star))
    val pred = IsIn("Num", ty)
    val sc = Forall(List(),Qual(List(pred),ty))
    val tvs = schemeTv(sc)
    printf("tvs = %s\n", tvs)
  }

  it should "quantify" in {
    val tyvar = Tyvar("a", Star)
    val ty = TVar(Tyvar("a", Star))
    val pred = IsIn("Num", ty)
    val qual = Qual(List(pred), fn(ty)(tInt))
    val sc = quantify(List(tyvar))(qual)
    printf("scheme = %s\n", sc)

  }

  it should "toScheme" in {
    val ty = TVar(Tyvar("a", Star))
    val sc = toScheme(ty)
    printf("scheme = %s\n", sc)
  }
}