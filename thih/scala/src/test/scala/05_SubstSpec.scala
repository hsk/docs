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

    // substは+->演算子で作れる
    val subst = Tyvar("a", Star) +-> tInt

    subst shouldBe List(
      (Tyvar("a", Star), TCon(Tycon("Int", Star))))

    val subst1 = Tyvar("b", Star) +-> tChar

    // substはリストなので ::: で結ß合出来る
    val subst2 = subst ::: subst1

    subst2 shouldBe List(
      (Tyvar("a", Star), TCon(Tycon("Int", Star))),
      (Tyvar("b", Star), TCon(Tycon("Char", Star))))

    // typeApplyはsubstを元に型変数がsubstにあれば置き換える。

    val tva = TVar(Tyvar("a", Star))
    val tvb = TVar(Tyvar("b", Star))
    typeApply(subst)(tva) shouldBe TCon(Tycon("Int", Star))
    typeApply(subst)(tvb) shouldBe TVar(Tyvar("b", Star))

    // TApの中身も置き換わる
    val tap = TAp(tva, tvb)
    typeApply(subst)(tap) shouldBe
      TAp(TCon(Tycon("Int", Star)), TVar(Tyvar("b", Star)))

    val tap2 = TAp(tva, tva)
    typeApply(subst)(tap2) shouldBe
      TAp(TCon(Tycon("Int", Star)), TCon(Tycon("Int", Star)))

    // typeTvでは内部で使っている型変数のリストを返す
    typeTv(tva) shouldBe List(Tyvar("a", Star))
    typeTv(tvb) shouldBe List(Tyvar("b", Star))

    // tapは2つの型を使っているのでaとbが返る
    typeTv(tap) shouldBe List(Tyvar("a", Star), Tyvar("b", Star))

    // listApply
    //def listApply[A,B](apply: Subst => A => B)(s : Subst)(xs:List[A]):List[B] = {
    //  xs.map(apply(s))
    //}

    // listApplyは複数の型を受け取って、展開する
    listApply { typeApply }(subst)(List(tva, tvb)) shouldBe List(
      TCon(Tycon("Int", Star)),
      TVar(Tyvar("b", Star)))

    // listTvはlist全体の内部で使っている型変数を求める
    // listTv
    //def listTv[A](tv: A => List[Tyvar])(xs:List[A]) : List[Tyvar] = {
    //  Pre.nub(xs.map(tv).flatten)
    //}
    listTv(typeTv)(List(tva, tap)) shouldBe
      List(
        Tyvar("a", Star),
        Tyvar("b", Star))

    // (@@)
    //implicit class SSubst(val s1: Subst) {
    //  def @@(s2 : Subst) : Subst = {
    //    s2.map {case (u, t) =>
    //      (u, typeApply(s1)(t))
    //    } ::: s1
    //  }
    //}
  }

  it should "@@" in {

    val subst = Tyvar("a", Star) +-> tInt
    val subst1 = Tyvar("b", Star) +-> TVar(Tyvar("a", Star)) ::: Tyvar("a", Star) +-> tChar
    val subst2 = Tyvar("b", Star) +-> TVar(Tyvar("a", Star)) ::: Tyvar("a", Star) +-> tInt

    // @@演算子で2つのsubstを結合出来る。最初のsubstをs2に実行して結合する
    (subst @@ subst1) shouldBe List(
      (Tyvar("b", Star), TCon(Tycon("Int", Star))),
      (Tyvar("a", Star), TCon(Tycon("Char", Star))),
      (Tyvar("a", Star), TCon(Tycon("Int", Star))))

    (subst @@ subst2) shouldBe List(
      (Tyvar("b", Star), TCon(Tycon("Int", Star))),
      (Tyvar("a", Star), TCon(Tycon("Int", Star))),
      (Tyvar("a", Star), TCon(Tycon("Int", Star))))
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

    try {
      // @@と似ているのだけど、s1とs2でおかしいものがあったらエラーにする
      merge(subst)(subst1)
      assert(false)
    } catch {
      case _: Throwable =>
    }

    merge(subst)(subst2) shouldBe List(
      (Tyvar("a", Star), TCon(Tycon("Int", Star))),
      (Tyvar("b", Star), TVar(Tyvar("a", Star))),
      (Tyvar("a", Star), TCon(Tycon("Int", Star))))

  }
}
