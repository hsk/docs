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
    IsIn("Num",TVar(Tyvar("a",Star)))

  }

  it should "pred list" in {
    val ty = TVar(Tyvar("a", Star))
    val preds = List(IsIn("Num", ty), IsIn("B", ty))

    preds shouldBe
    List(
      IsIn("Num",TVar(Tyvar("a",Star))),
      IsIn("B",TVar(Tyvar("a",Star)))
    )
  }

  it should "pred & qual" in {
    // (Num a) => a -> Int

    val ty = TVar(Tyvar("a", Star))
    val pred = IsIn("Num", ty)
    pred shouldBe IsIn("Num",TVar(Tyvar("a",Star)))

    // Qual
    val q = Qual(List(pred), fn(ty)(tInt))
    q shouldBe Qual(List(IsIn("Num",TVar(Tyvar("a",Star)))),
      TAp(TAp(TCon(Tycon("(=>)",Kfun(Star,Kfun(Star,Star)))),
         TVar(Tyvar("a",Star))),TCon(Tycon("Int",Star))))
  }

  it should "predApply" in {
    val s = List((Tyvar("a", Star), tInt))
    val pred = IsIn("Num", TVar(Tyvar("a", Star)))
    val pred2 = predApply(s)(pred)
    pred2 shouldBe IsIn("Num", TCon(Tycon("Int",Star)))

  }

  it should "predTv" in {
    val pred = IsIn("Num", TVar(Tyvar("a", Star)))
    val tvs = predTv(pred)
    tvs shouldBe List(Tyvar("a",Star))

  }
  it should "predsApply" in {
    val s = List((Tyvar("a", Star), tInt))
    val preds = List(IsIn("Num", TVar(Tyvar("a", Star))))
    val preds2 = predsApply(s)(preds)
    preds2 shouldBe List(IsIn("Num",TCon(Tycon("Int",Star))))

  }
  it should "predsTv" in {

    val preds = List(IsIn("Num", TVar(Tyvar("a", Star))))
    val tvs = predsTv(preds)
    printf("tvs = %s\n", tvs)
    tvs shouldBe List(Tyvar("a",Star))
  }
  it should "qualTypeApply" in {

    val ty = TVar(Tyvar("a", Star))
    val pred = IsIn("Num", ty)
    val q = Qual(List(pred), fn(ty)(tInt))
    printf("qual = %s\n", q)
    val qual2 = qualTypeApply(Tyvar("a", Star) +-> tInt)(q)
    printf("qual2 = %s\n", qual2)
    q shouldBe Qual(
      List(
        IsIn("Num",TVar(Tyvar("a",Star)))
      ),
      TAp(TAp(TCon(Tycon("(=>)",Kfun(Star,Kfun(Star,Star)))),TVar(Tyvar("a",Star))),TCon(Tycon("Int",Star)))
    )

    qual2 shouldBe Qual(
      List(
        IsIn("Num",TCon(Tycon("Int",Star)))
      ),
      TAp(TAp(TCon(Tycon("(=>)",Kfun(Star,Kfun(Star,Star)))),TCon(Tycon("Int",Star))),TCon(Tycon("Int",Star)))
    )

  }
  it should "qualTypeTv" in {

    val ty = TVar(Tyvar("a", Star))
    val pred = IsIn("Num", ty)
    val q = Qual(List(pred), fn(ty)(tInt))
    val tvs = qualTypeTv(q)
    tvs shouldBe List(Tyvar("a",Star))

  }
  it should "mguPred" in {
    val pred1 = IsIn("Num", TVar(Tyvar("a", Star)))
    val pred2 = IsIn("Num", TVar(Tyvar("a", Star)))
    val s = mguPred(pred1)(pred2)
    s shouldBe List()
  }
  it should "matchPred" in {
    val pred1 = IsIn("Num", TVar(Tyvar("a", Star)))
    val pred2 = IsIn("Num", TVar(Tyvar("a", Star)))
    val s = matchPred(pred1)(pred2)
    s shouldBe List((Tyvar("a",Star),TVar(Tyvar("a",Star))))
  }

  it should "Inst" in {
    val inst = Qual(
      List(IsIn("Ord",tUnit),IsIn("Ord",tChar)),IsIn("Ord",tChar))
    
    inst shouldBe Qual(
      List(
        IsIn("Ord",TCon(Tycon("()",Star))),
        IsIn("Ord",TCon(Tycon("Char",Star)))
      ),
      IsIn("Ord",TCon(Tycon("Char",Star)))
    )
  }

  it should "class_ :=>" in {

    val cls:Class_ = (
      List("Eq"),
      List(
        List() :=> IsIn("Ord", tUnit),
        List() :=> IsIn("Ord", tChar),
        List() :=> IsIn("Ord",tInt),
        List(
          IsIn("Ord",TVar(Tyvar("a", Star))),
          IsIn("Ord",TVar(Tyvar("b", Star)))
        ) :=>
        IsIn("Ord", (pair (TVar(Tyvar("a",Star))) (TVar(Tyvar("b",Star)))))
        
      )
    )

  }

  // 7.2 Class Environments

  it should "modify" in {
    val b = modify(initialEnv)("ABC")(List("A"),
      List(List() :=> IsIn("Ord",tUnit)))
    printf("modify b=%s\n",b)
    //b.toString shouldBe "ClassEnv(<function1>,List(TCon(Tycon(Integer,Star)), TCon(Tycon(Double,Star))))"
  }
  it should "super_" in {
    val b = modify(initialEnv)("ABC")(List("A"),
      List(List() :=> IsIn("Ord",tUnit)))
    val s = super_(b)("ABC")
    printf("super_ s=%s\n",b)
    //super_ s=ClassEnv(<function1>,List(TCon(Tycon(Integer,Star)), TCon(Tycon(Double,Star))))
  }
  it should "insts" in {
    val b = modify(initialEnv)("ABC")(List("A"),
      List(List() :=> IsIn("Ord",tUnit)))
    val s = insts(b)("ABC")

  }
  it should "defined" in {
    val b = modify(initialEnv)("ABC")(List("A"),
      List(List() :=> IsIn("Ord",tUnit)))
    val s = defined(b)("ABC")
    s shouldBe true
  }
  it should "addClass" in {
    val c1 :EnvTransformer = addClass("Eq")(List())
    val c1s = c1(initialEnv)
    printf("addClass c1s %s\n", c1s)


    // addClass c1s ClassEnv(<function1>,List(TCon(Tycon(Integer,Star)), TCon(Tycon(Double,Star))))

  }

  it should "<:>" in {
    val c1 :EnvTransformer = addClass("Eq")(List())
    val c2 :EnvTransformer = addClass("Eq2")(List())
    val c3 :EnvTransformer = c1 <:> c2
    val c4 :EnvTransformer = addClass("Eq")(List()) <:>
                             addClass("Eq2")(List())
    printf("<:> c4 %s\n", c4)
    // <:> c4 <function1>
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
      IsIn("Num",TVar(Tyvar("a",Star))),
      IsIn("Eq",TVar(Tyvar("a",Star))),
      IsIn("Show",TVar(Tyvar("a",Star))))

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
    val r = inHnf(IsIn("Num",TVar(Tyvar("a", Star))))
    r shouldBe true
    val r2 = inHnf(IsIn("Num",tInt))
    r2 shouldBe false
  }

  it should "toHnfs" in {
    val preds = List(IsIn("Num",TVar(Tyvar("a", Star))))
    val preds2 = toHnfs(initialEnv)(preds)
    preds2 shouldBe List(IsIn("Num",TVar(Tyvar("a", Star))))
  }

  it should "toHnf" in {
    val pred = IsIn("Num",TVar(Tyvar("a", Star)))
    val preds = toHnf(initialEnv)(pred)
    preds shouldBe List(IsIn("Num",TVar(Tyvar("a", Star))))
  }

  it should "simplify" in {
    val pred = IsIn("Num", TVar(Tyvar("a", Star)))
    val preds = List(pred)
    val preds2 = simplify(exampleInsts(initialEnv))(preds)
    preds2 shouldBe List(IsIn("Num",TVar(Tyvar("a", Star))))
  }

  it should "reduce" in {
    val pred = IsIn("Num", TVar(Tyvar("a", Star)))
    val preds = List(pred)
    val preds2 = reduce(exampleInsts(initialEnv))(preds)
    preds2 shouldBe List(IsIn("Num",TVar(Tyvar("a", Star))))
  }

  it should "scEntail" in {
    val pred = IsIn("Num", TVar(Tyvar("a", Star)))
    val preds = List(pred)
    val result = scEntail(exampleInsts(initialEnv))(preds)(pred)
    result shouldBe true
  }
}