import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import thih._

class PredSpec extends FlatSpec {
  import Type._
  import Pred._
  import Kind._

  it should "pred" in {
    val ty = TVar(Tyvar("a", Star))
    val pred = IsIn("Num", ty)

    pred shouldBe
      IsIn("Num", TVar(Tyvar("a", Star)))

  }

  it should "pred list" in {
    val ty = TVar(Tyvar("a", Star))
    val preds = List(IsIn("Num", ty), IsIn("B", ty))

    preds shouldBe
      List(
        IsIn("Num", TVar(Tyvar("a", Star))),
        IsIn("B", TVar(Tyvar("a", Star))))
  }

  it should "pred & qual" in {
    // (Num a) => a -> Int

    val ty = TVar(Tyvar("a", Star))
    val pred = IsIn("Num", ty)
    pred shouldBe IsIn("Num", TVar(Tyvar("a", Star)))

    // Qual
    val q = Qual(List(pred), fn(ty)(tInt))
    q shouldBe Qual(List(IsIn("Num", TVar(Tyvar("a", Star)))),
      TAp(TAp(TCon(Tycon("(=>)", Kfun(Star, Kfun(Star, Star)))),
        TVar(Tyvar("a", Star))), TCon(Tycon("Int", Star))))
  }

  it should "predApply" in {
    val subst = List((Tyvar("a", Star), tInt))
    val pred = IsIn("Num", TVar(Tyvar("a", Star)))
    val pred2 = predApply(subst)(pred)
    pred2 shouldBe IsIn("Num", TCon(Tycon("Int", Star)))

  }

  it should "predTv" in {
    val pred = IsIn("Num", TVar(Tyvar("a", Star)))
    val tvs = predTv(pred)
    tvs shouldBe List(Tyvar("a", Star))

  }

  it should "predsApply" in {
    val subst = List((Tyvar("a", Star), tInt))
    val preds = List(IsIn("Num", TVar(Tyvar("a", Star))))
    val preds2 = predsApply(subst)(preds)
    preds2 shouldBe List(IsIn("Num", TCon(Tycon("Int", Star))))

  }
  it should "predsTv" in {

    val preds = List(IsIn("Num", TVar(Tyvar("a", Star))))
    val tvs = predsTv(preds)
    tvs shouldBe List(Tyvar("a", Star))
  }
  it should "qualTypeApply" in {

    val subst = Tyvar("a", Star) +-> tInt
    val ty = TVar(Tyvar("a", Star))
    val pred = IsIn("Num", ty)
    val qual = Qual(List(pred), fn(ty)(tInt))
    val qual2 = qualTypeApply(subst)(qual)

    qual shouldBe Qual(
      List(
        IsIn("Num", TVar(Tyvar("a", Star)))),
      TAp(TAp(TCon(Tycon("(=>)", Kfun(Star, Kfun(Star, Star)))), TVar(Tyvar("a", Star))), TCon(Tycon("Int", Star))))

    qual2 shouldBe Qual(
      List(
        IsIn("Num", TCon(Tycon("Int", Star)))),
      TAp(TAp(TCon(Tycon("(=>)", Kfun(Star, Kfun(Star, Star)))), TCon(Tycon("Int", Star))), TCon(Tycon("Int", Star))))

  }
  it should "qualTypeTv" in {

    val ty = TVar(Tyvar("a", Star))
    val pred = IsIn("Num", ty)
    val qual = Qual(List(pred), fn(ty)(tInt))
    val tvs = qualTypeTv(qual)
    tvs shouldBe List(Tyvar("a", Star))

  }
  it should "mguPred" in {
    val pred1 = IsIn("Num", TVar(Tyvar("a", Star)))
    val pred2 = IsIn("Num", tInt)

    val subst = mguPred(pred1)(pred2)
    subst shouldBe
      List((Tyvar("a",Star),TCon(Tycon("Int",Star))))

    val subst2 = mguPred(pred1)(pred1)
    subst2 shouldBe
      List()

  }
  it should "matchPred" in {
    val pred1 = IsIn("Num", TVar(Tyvar("a", Star)))
    val pred2 = IsIn("Num", tInt)

    val subst = matchPred(pred1)(pred2)
    subst shouldBe List((Tyvar("a", Star), tInt))

    val subst2 = matchPred(pred1)(pred1)
    subst2 shouldBe List((Tyvar("a", Star), TVar(Tyvar("a", Star))))
  }

  it should "Inst" in {
    val inst = Qual(
      List(
        IsIn("Ord", tUnit),
        IsIn("Ord", tChar)),
      IsIn("Ord", tChar))

    inst shouldBe Qual(
      List(
        IsIn("Ord", TCon(Tycon("()", Star))),
        IsIn("Ord", TCon(Tycon("Char", Star)))),
      IsIn("Ord", TCon(Tycon("Char", Star))))
  }

  it should "class_ :=>" in {

    val cls: Class_ = (
      List("Eq"),
      List(
        List() :=> IsIn("Ord", tUnit),
        List() :=> IsIn("Ord", tChar),
        List() :=> IsIn("Ord", tInt),
        List(
          IsIn("Ord", TVar(Tyvar("a", Star))),
          IsIn("Ord", TVar(Tyvar("b", Star)))) :=>
          IsIn("Ord", (pair(TVar(Tyvar("a", Star)))(TVar(Tyvar("b", Star)))))))

  }

  // 7.2 Class Environments

  it should "modify" in {
    val ce: ClassEnv = modify(initialEnv)("ABC")(List("A"),
      List(List() :=> IsIn("Ord", tUnit)))

    ce.defaults shouldBe
      List(TCon(Tycon("Integer", Star)), TCon(Tycon("Double", Star)))
  }

  it should "super_" in {
    val ce = modify(initialEnv)("ABC")(List("A"),
      List(List() :=> IsIn("Ord", tUnit)))
    val s = super_(ce)("ABC")
    s shouldBe List("A")
  }

  it should "insts" in {
    val ce = modify(initialEnv)("ABC")(List("A"),
      List(List() :=> IsIn("Ord", tUnit)))
    val s = insts(ce)("ABC")
    s shouldBe List(Qual(List(), IsIn("Ord", TCon(Tycon("()", Star)))))

  }

  it should "defined" in {
    val ce = modify(initialEnv)("ABC")(List("A"),
      List(List() :=> IsIn("Ord", tUnit)))
    val s = defined(ce)("ABC")
    s shouldBe true
  }

  it should "addClass" in {
    val et: EnvTransformer = addClass("Eq")(List())
    val ce = et(initialEnv)

    ce.defaults shouldBe
      List(TCon(Tycon("Integer", Star)), TCon(Tycon("Double", Star)))
  }

  it should "<:>" in {
    val et1: EnvTransformer = addClass("Eq")(List())
    val et2: EnvTransformer = addClass("Eq2")(List())
    val et3: EnvTransformer = et1 <:> et2

    et3(initialEnv).defaults shouldBe
      List(TCon(Tycon("Integer", Star)), TCon(Tycon("Double", Star)))

    val et4: EnvTransformer = addClass("Eq")(List()) <:>
      addClass("Eq2")(List())

    et4(initialEnv).defaults shouldBe
      List(TCon(Tycon("Integer", Star)), TCon(Tycon("Double", Star)))
  }

  it should "overlap" in {

    val pred1 = IsIn("Ord", tUnit)
    val pred2 = IsIn("Ord", tChar)
    overlap(pred1)(pred2) shouldBe false
    overlap(pred1)(pred1) shouldBe true
  }

  // 7.3 Entailment

  it should "bySuper" in {
    val preds = bySuper(exampleInsts(initialEnv))(IsIn("Num", TVar(Tyvar("a", Star))))
    preds shouldBe List(
      IsIn("Num", TVar(Tyvar("a", Star))),
      IsIn("Eq", TVar(Tyvar("a", Star))),
      IsIn("Show", TVar(Tyvar("a", Star))))

  }
  it should "byInst" in {
    val preds = byInst(exampleInsts(initialEnv))(IsIn("Num", TVar(Tyvar("a", Star))))
    preds shouldBe None
  }

  it should "entail" in {
    val p = IsIn("Num", TVar(Tyvar("a", Star)))
    val ps = List(p)
    val result = entail(exampleInsts(initialEnv))(ps)(p)
    result shouldBe true
  }

  // 7.4 Context Reduction

  it should "inHnf" in {
    val r = inHnf(IsIn("Num", TVar(Tyvar("a", Star))))
    r shouldBe true
    val r2 = inHnf(IsIn("Num", tInt))
    r2 shouldBe false
  }

  it should "toHnfs" in {
    val preds = List(IsIn("Num", TVar(Tyvar("a", Star))))
    val preds2 = toHnfs(initialEnv)(preds)
    preds2 shouldBe List(IsIn("Num", TVar(Tyvar("a", Star))))
  }

  it should "toHnf" in {
    val pred = IsIn("Num", TVar(Tyvar("a", Star)))
    val preds = toHnf(initialEnv)(pred)
    preds shouldBe List(IsIn("Num", TVar(Tyvar("a", Star))))
  }

  it should "simplify" in {
    val pred = IsIn("Num", TVar(Tyvar("a", Star)))
    val preds = List(pred)
    val preds2 = simplify(exampleInsts(initialEnv))(preds)
    preds2 shouldBe List(IsIn("Num", TVar(Tyvar("a", Star))))
  }

  it should "reduce" in {
    val pred = IsIn("Num", TVar(Tyvar("a", Star)))
    val preds = List(pred)
    val preds2 = reduce(exampleInsts(initialEnv))(preds)
    preds2 shouldBe List(IsIn("Num", TVar(Tyvar("a", Star))))
  }

  it should "scEntail" in {
    val pred = IsIn("Num", TVar(Tyvar("a", Star)))
    val preds = List(pred)
    val result = scEntail(exampleInsts(initialEnv))(preds)(pred)
    result shouldBe true
  }
}
