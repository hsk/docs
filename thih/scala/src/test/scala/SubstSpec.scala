import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import thih._

class SubstSpec extends FlatSpec {
  import Kind._
  import Type._
  import Subst._

  it should "nullSubst" in {
    Subst.nullSubst shouldBe Subst.nullSubst 
  }

  it should "+->" in {

    // +->
    val subst = Tyvar("a", Star) +-> tInt

    subst shouldBe List(
      (Tyvar("a",Kind.Star),TCon(Tycon("Int",Kind.Star)))
    )
    val subst1 = Tyvar("b", Star) +-> tChar
    val subst2 = subst ::: subst1

    subst2 shouldBe List(
      (Tyvar("a",Kind.Star),TCon(Tycon("Int",Kind.Star))),
      (Tyvar("b",Kind.Star),TCon(Tycon("Char",Kind.Star)))
    )

    val tva = TVar(Tyvar("a", Star))
    val tvb = TVar(Tyvar("b", Star))
    typeApply(subst)(tva) shouldBe TCon(Tycon("Int",Kind.Star))
    typeApply(subst)(tvb) shouldBe TVar(Tyvar("b",Kind.Star))

    val tap = TAp(tva,tvb)
    typeApply(subst)(tap) shouldBe
    TAp(TCon(Tycon("Int",Kind.Star)),TVar(Tyvar("b",Kind.Star)))

    typeTv(tva) shouldBe List(Tyvar("a",Kind.Star))
    typeTv(tvb) shouldBe List(Tyvar("b",Kind.Star))
    typeTv(tap) shouldBe List(Tyvar("a",Kind.Star), Tyvar("b",Kind.Star))

    // listApply
    //def listApply[A,B](apply: Subst => A => B)(s : Subst)(xs:List[A]):List[B] = {
    //  xs.map(apply(s))
    //}

    listApply{typeApply}(subst)(List(tva,tvb)) shouldBe List(
      TCon(Tycon("Int",Kind.Star)),
      TVar(Tyvar("b",Kind.Star))
    )

    // listTv
    //def listTv[A](tv: A => List[Tyvar])(xs:List[A]) : List[Tyvar] = {
    //  Pre.nub(xs.map(tv).flatten)
    //}
    listTv(typeTv)(List(tva,tap)) shouldBe
    List(
      Tyvar("b",Kind.Star),
      Tyvar("a",Kind.Star)
    )

    // (@@)
    //implicit class SSubst(val s1: Subst) {
    //  def @@(s2 : Subst) : Subst = {
    //    s2.map {case (u, t) =>
    //      (u, typeApply(s1)(t))
    //    } ::: s1
    //  }
    //}


    (subst @@ subst1) shouldBe List(
      (Tyvar("b",Kind.Star),TCon(Tycon("Char",Kind.Star))),
      (Tyvar("a",Kind.Star),TCon(Tycon("Int",Kind.Star)))
    )

    // merge
    //def merge(s1:Subst)(s2:Subst) : Subst = {
    //  val agree = {
    //    Pre.intersect(s1.map(_._1))(s2.map(_._1)).forall{ v =>
    //      typeApply(s1)(TVar(v)) == typeApply(s2)(TVar(v))
    //    }
    //  }
    //  if(agree) s1 ::: s2
    //  else throw new Exception("substitutions do not agree")
    //}


    merge(subst)(subst1) shouldBe List(
      (Tyvar("a",Kind.Star),TCon(Tycon("Int",Kind.Star))),
      (Tyvar("b",Kind.Star),TCon(Tycon("Char",Kind.Star)))
    )

  }
}
