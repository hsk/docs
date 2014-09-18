import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import thih._

class AssumpSpec extends FlatSpec {

  import Scheme._
  import Type._
  import Kind._
  import Assump._

  it should "assump" in {
    val t = TVar(Tyvar("a", Star))
    val assump = Assump("ABC", Forall(List(),Pred.Qual(List(),t)))
    printf("show %s\n", assump)
  }

  it should "assumpApply" in {
    val t = TVar(Tyvar("a", Star))
    val assump = Assump("ABC", Forall(List(),Pred.Qual(List(),t)))
    val subst:Subst.Subst = List((Tyvar("a", Star), tInt))
    val assump2 = assumpApply(subst)(assump)
    printf("assumpApply %s\n", assump2)
  }

  it should "assumpTv" in {
    val t = TVar(Tyvar("a", Star))
    val assump = Assump("ABC", Forall(List(),Pred.Qual(List(),t)))
    val tvs = assumpTv(assump)
    printf("assumpTv %s\n", tvs)
  }
  
  it should "assumpsApply" in {
    val t = TVar(Tyvar("a", Star))
    val assump = Assump("ABC", Forall(List(),Pred.Qual(List(),t)))
    val subst = List((Tyvar("a", Star), tInt))
    val assumps = assumpsApply(subst)(List(assump))
    printf("show %s\n", assumps)
  }

  it should "assumpsTv" in {
    val t = TVar(Tyvar("a", Star))
    val assump = Assump("ABC", Forall(List(),Pred.Qual(List(),t)))
    val tvs = assumpsTv(List(assump))
    printf("assumpsTv %s\n", tvs)
  }

  it should "find" in {
    val t = TVar(Tyvar("a", Star))
    val assump = Assump("ABC", Forall(List(),Pred.Qual(List(),t)))
    val sc = find("ABC")(List(assump))
    printf("find %s\n", sc)
  }

}