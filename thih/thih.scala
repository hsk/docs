/*

 This file is a translation and changing of the code pertaining to the article
 `Typing Haskell in Haskell' by Mark P. Jones [1], from Haskell to OCaml.
 Original OCaml file made by Cyril Soldani [2]. 

 1: http://web.cecs.pdx.edu/~mpj/thih/
 2: http://devmusings.legiasoft.com/_media/blog/2010/08/04/typinghaskellinml.ml
 
 Copyright (C) 1999 - 2000  Mark P. Jones
 Copyright (C) 2010 Cyril Soldani
 Copyrigtt (C) 2014 Hiroshi Sakurai

 Here follows the license (license of the original Haskell code from the
 article).

 `Typing Haskell in Haskell' is Copyright (c) Mark P Jones
 and the Oregon Graduate Institute of Science and Technology,
 1999-2000, All rights reserved, and is distributed as
 free software under the following license.

 Redistribution and use in source and binary forms, with or
 without modification, are permitted provided that the following
 conditions are met:

 - Redistributions of source code must retain the above copyright
 notice, this list of conditions and the following disclaimer.

 - Redistributions in binary form must reproduce the above
 copyright notice, this list of conditions and the following
 disclaimer in the documentation and/or other materials provided
 with the distribution.

 - Neither name of the copyright holders nor the names of its
 contributors may be used to endorse or promote products derived
 from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND THE
 CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR THE
 CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package thih

// 2 Preliminaries
// 2 予備知識
object Pre extends App{
  // 和集合
  def union[A] (xs: List[A]) (ys: List[A]):List[A] = {
    xs.filter { !ys.contains(_) } ::: ys
  }

  // 積集合
  def intersect[A] (xs: List[A]) (ys: List[A]): List[A] = {
    xs.filter { ys.contains(_) }
  }

  // リストをセットにする。要素が１つずつにまとめる
  def nub[A](xs: List[A]): List[A] = {
    xs.foldLeft(List[A]()){case(ys,y) =>
      if(ys.contains(y)) ys
      else y :: ys
    }
  }

  // 空チェック
  def isEmpty[A](xs: List[A]):Boolean = {
    xs match {
      case List() => true
      case _ => false
    }      
  }


  // reduceじゃないのかな
  def fold_left1[A](f:(A, A) => A)(xs:List[A]): A = {
    xs match {
      case List() => throw new Exception("empty list") 
      case List(x) => x
      case x :: xs => xs.foldLeft(x)(f)
    }
  }

  // リスト内の最初の1個目のxを削除する
  def deleteFirst[A](x:A)(ys:List[A]): List[A] = {
    ys match {
      case List() => List()
      case y :: ys =>
        if (x == y) ys
        else y :: deleteFirst(x)(ys)
    }
  }

  // 最初のリストから2番目のリストの要素を消す
  def diff[A](xs:List[A])(ys:List[A]): List[A] = {
    ys match {
      case List() => xs
      case y :: ys => diff(deleteFirst(y)(xs))(ys)
    }
  }

  // 3つの多値を持っているリストを３つのリストに分割する
  def split3[A,B,C](xs:List[(A, B, C)]):(List[A], List[B], List[C]) = {
    def loop(ws:List[A], xs:List[B], ys:List[C])
      (zs:List[(A, B, C)]):(List[A],List[B],List[C]) = {
      zs match {
        case List() => (ws.reverse, xs.reverse, ys.reverse)
        case (w, x, y) :: zs => loop(w :: ws, x :: xs, y :: ys)(zs)
      }
    }    
    loop(List(), List(), List())(xs)
  }

  {
    val a = List(5,4,3,2,1)
    val b = List(4,5,6,7)

    {
      val ab = Pre.union(a)(b)
      printf("union a b = %s\n", ab)
    }

    val ab = Pre.intersect(a)(b)
    printf("intersect a b = %s\n", ab)
  }

  {
    val a = List(1,1,2,2,3,4,5,1)
    val b = Pre.nub(a)
    printf("nub a = %s\n", b)
  }

  {
    val a = List()
    val b = Pre.isEmpty(a)
    printf("isEmpty = %b\n", b)
  }
  {
    val a = List(1)
    val b = Pre.isEmpty(a)
    printf("isEmpty = %b\n", a)
  }

  {
    val a = List(1,2,3,4,5,6,7,8,9,10)
    val b = Pre.fold_left1[Int]{case(a, b) =>
      a + b
    }(a)

    printf("fold_left1 = %d\n", b)
  }


  {
    val a = List(1,2,3,4,3,4,5)
    val b = Pre.deleteFirst(3)(a)
    printf("deleteFirst = %s\n", b)
  }
  {
    val a = List(1,2,3,4)
    val b = List(3,4,5,6)
    val r = Pre.diff(a)(b)
    printf("diff = %s\n", r)
  }
  {
    val a = List(1,2,3,4,3,4)
    val b = List(3,4,5,6,4)
    val r = Pre.diff(a)(b)
    printf("diff = %s\n", r)
  }


  {
    val a1 = List((1,10,100),(2,20,200),(3,30,300))
    val (a,b,c) = Pre.split3(a1)
    printf("split3 = %s;%s;%s\n", a, b, c)
  }
}

object Id extends App {
  type Id = String

  // 数値に対するidを取得する
  def enumId (n:Int) : Id = {
    "v" + n
  }

  {
    printf("id=%s\n", enumId(33))
  }
}


// 3 Kinds
object Kind {
  sealed trait Kind
  object Star extends Kind
  case class Kfun(a:Kind,b:Kind) extends Kind
}


// 4 Types
object Type {
  import Kind._

  // 型変数
  case class Tyvar(a:Id.Id, b: Kind) {
    def +->(t:Type_) : Subst.Subst = {
      List((this, t))
    }
  }

  // 型コンストラクタ
  case class Tycon(a:Id.Id, b: Kind)

  // 型
  sealed trait Type_

  case class TVar(a:Tyvar) extends Type_
  case class TCon(a:Tycon) extends Type_
  case class TAp(a:Type_, b:Type_) extends Type_
  case class TGen(a:Int) extends Type_

  def tUnit :Type_ = TCon(Tycon("()", Star))
  def tChar :Type_ = TCon(Tycon("Char", Star))
  def tInt :Type_ = TCon(Tycon("Int", Star))
  def tInteger :Type_ = TCon(Tycon("Integer", Star))
  def tFloat :Type_ = TCon(Tycon("Float", Star))
  def tDouble :Type_ = TCon(Tycon("Double", Star))

  def tList :Type_ = TCon(Tycon("List()", Kfun(Star, Star)))
  def tArrow :Type_ = TCon(Tycon("(=>)", Kfun(Star, Kfun(Star, Star))))
  def tTuple2 :Type_ = TCon(Tycon("(,)", Kfun(Star, Kfun(Star, Star))))

  def fn(a:Type_)(b:Type_) :Type_ = TAp(TAp(tArrow, a), b)

  def list(t:Type_):Type_ = TAp(tList, t)

  def tString :Type_ = list(tChar)

  def pair(a:Type_)(b:Type_) :Type_ = TAp(TAp(tTuple2, a), b)

  def tyvarKind (tv:Tyvar):Kind = {
    tv match {
      case Tyvar(_, k) => k
    }
  }

  def tyconKind (tc:Tycon):Kind = {
    tc match {
      case Tycon(_, k) => k
    }
  }

  def typeKind(t:Type_):Kind = {
    t match {
      case TCon(tc) => tyconKind(tc)
      case TVar(u) => tyvarKind(u)
      case TAp(t, _) =>
        typeKind(t) match {
          case Kfun(_, k) => k
          case _ => throw new Exception("inconsistent type")
        }
      case TGen(_) => throw new Exception("generic type variables have no kind")
    }
  }
}

// 5 Substitutions
object Subst {
  import Type._

  type Subst = List[(Tyvar,Type_)]

  def nullSubst : Subst = List()

  // 型変数を展開する
  def typeApply(s : Subst)(t:Type_):Type_ = {
    t match {
      case TVar(u) =>
        s.find{case (k,v) => k == u} match {
          case Some((_,t1)) => t1
          case _ => t
        }
      case TAp(l, r) => TAp(typeApply(s)(l), typeApply(s)(r))
      case t => t
    }
  }

  def typeTv(t:Type_):List[Tyvar]= {
    t match {
      case TVar(u) => List(u)
      case TAp(l, r) => Pre.union(typeTv(l))(typeTv(r))
      case _ => List()
    }
  }


  def listApply[A,B] (apply : Subst => A => B) (s : Subst) (xs:List[A]):List[B] = {
    xs.map(apply(s))
  }


  def listTv[A] (tv:A => List[Tyvar]) (xs:List[A]) : List[Tyvar] = {
    Pre.nub(xs.map(tv).flatten)
  }

  implicit class SSubst(val s1: Subst) {
    def @@ (s2 : Subst) : Subst = {
      s2.map {case (u, t) =>
        (u, typeApply(s1)(t))
      } ::: s1
    }
  }

  def merge(s1:Subst)(s2:Subst) : Subst = {
    val agree = {
      Pre.intersect(s1.map(_._1))(s2.map(_._1)).forall{ v =>
        typeApply(s1)(TVar(v)) == typeApply(s2)(TVar(v))
      }
    }
    if(agree) s1 ::: s2
    else throw new Exception("substitutions do not agree")
  }
}

// 6 Unification and Matching
object Unify {
  import Kind._
  import Type._
  import Subst._

  def mgu (t1:Type_) (t2:Type_):Subst = {
    (t1, t2) match {
      case (TAp(l, r), TAp(l_, r_)) =>
        val s1 = mgu(l)(l_)
        val s2 = mgu(typeApply(s1)(r))(typeApply(s1)(r_))
        s2 @@ s1
      case (TVar(u), t) => varBind(u)(t)
      case (t, TVar(u)) => varBind(u)(t)
      case (TCon(tc1), TCon(tc2)) if tc1 == tc2 => nullSubst
      case _ => throw new Exception("types do not unify")
    }
  }

  def varBind (u:Tyvar) (t:Type_):Subst = {
    if (t == TVar(u)) nullSubst
    else if (typeTv(t).contains(u)) throw new Exception("occurs check fails")
    else if (tyvarKind(u) != typeKind(t)) throw new Exception("kinds do not match")
    else u +-> t
  }


  def match_(t1:Type_)(t2:Type_):Subst = {
    (t1,t2) match {
      case (TAp(l, r), TAp(l_, r_)) =>
        val sl = match_(l)(l_)
        val sr = match_(r)(r_)
        merge(sl)(sr)
      case (TVar(u), t) if (tyvarKind(u) == typeKind(t)) =>
        u +-> t
      case (TCon(tc1), TCon(tc2)) if (tc1 == tc2) =>
        nullSubst
      case _ =>
        throw new Exception("types do not match")
    }
  }
}


// 7 Type Classes, Predicates and Qualified Types
object Pred {
  import Kind._
  import Type._
  import Subst._

  // 7.1 Basic definitions
  case class IsIn(a:Id.Id, b:Type_)
  type Pred = IsIn

  case class Qual[T](a:List[Pred],t:T)

  def predApply (s:Subst) (pred:Pred):Pred = {
    pred match {
      case IsIn(i, t) => IsIn(i, Subst.typeApply(s)(t))
    }
  }

  def predsApply (s:Subst) (xs:List[Pred]):List[Pred] = {
    Subst.listApply(predApply)(s)(xs)
  }

  def qualTypeApply (s:Subst) (qual:Qual[Type_]):Qual[Type_] = {
    qual match {
      case Qual(ps, t) => Qual(predsApply(s)(ps), Subst.typeApply(s)(t))
    }
  }

  def predTv(pred:Pred):List[Tyvar] = {
    pred match {
      case IsIn(_, t) => Subst.typeTv(t)
    }
  }

  def predsTv(xs:List[Pred]) : List[Tyvar] = {
    Subst.listTv(predTv)(xs)
  }

  def qualTypeTv(qual:Qual[Type_]):List[Tyvar] = {
    qual match {
      case Qual(ps, t) =>
        Pre.union (predsTv(ps)) (Subst.typeTv(t))
    }
  }

  def lift[A] (m:Type_ =>Type_ =>A) (p:Pred) (p_ :Pred):A = {
    (p, p_) match {
      case (IsIn(i, t), IsIn(i_, t_)) =>
        if (i == i_) m(t)(t_)
        else throw new Exception("classes differ")
    }
  }

  def mguPred(p:Pred)(p_ :Pred):Subst = {
    lift(Unify.mgu)(p)(p_)
  }
  def matchPred(p:Pred)(p_ :Pred) :Subst = {
    lift(Unify.match_)(p)(p_)
  }

  type Inst = Qual[Pred]
  type Class_ = (List[Id.Id], List[Inst])

  // 7.2 Class Environments

  case class ClassEnv(
    classes : Id.Id => Class_,
    defaults : List[Type_]
  )


  def super_ (ce:ClassEnv)(i:Id.Id) = {
    ce.classes(i)._1
  }

  def insts (ce:ClassEnv)(i:Id.Id) = {
    ce.classes(i)._2
  }
  
  def defined (ce:ClassEnv)(i:Id.Id):Boolean = {
    try {
      ce.classes(i)
      true
    } catch {
      case _: Throwable => false
    }
  }

  def modify (ce:ClassEnv)(i:Id.Id)(c:Class_) = {
    ce.copy(
      classes = {j =>
        if (i == j) c else ce.classes(j)
      }
    )
  }

  def initialEnv :ClassEnv = {
    ClassEnv(
      {i => throw new Exception("Not found")},
      List(tInteger, tDouble)
    )
  }


  type EnvTransformer = ClassEnv => ClassEnv

  implicit class EEnvTransformar(val f:EnvTransformer) {
    def <:> (g : EnvTransformer) : EnvTransformer = {
      (ce:ClassEnv) => g(f(ce))
    }
  }

  def addClass(i:Id.Id)(is:List[Id.Id])  : EnvTransformer = { (ce:ClassEnv) =>
    if (defined(ce)(i)) throw new Exception("class already defined")
    if (is.exists{i => !defined(ce)(i) })
      throw new Exception("superclass not defined")
    modify(ce)(i)(is, List())
  }


  def addCoreClasses :EnvTransformer =
    addClass("Eq")(List[Id.Id]()) <:>
    addClass("Ord")(List("Eq"))   <:>
    addClass("Show")(List())      <:>
    addClass("Read")(List())      <:>
    addClass("Bounded")(List())   <:>
    addClass("Enum")(List())      <:>
    addClass("Functor")(List())   <:>
    addClass("Monad")(List())


  def addNumClasses :EnvTransformer =
    addClass("Num")(List("Eq", "Show"))                 <:>
    addClass("Real")(List("Num", "Ord"))                <:>
    addClass("Fractional")(List("Num"))                 <:>
    addClass("Integral")(List("Real", "Enum"))          <:>
    addClass("RealFrac")(List("Real", "Fractional"))    <:>
    addClass("Floating")(List("Fractional"))            <:>
    addClass("RealFloat")(List("RealFrac", "Floating"))

  def addPreludeClasses :EnvTransformer =
    addCoreClasses <:> addNumClasses

  def overlap (p:Pred) (q:Pred) : Boolean = {
    try {
      mguPred(p)(q)
      true
    } catch {
      case _ : Throwable => false
    }
  }

  def addInst(ps:List[Pred])(p:Pred) : EnvTransformer = { (ce:ClassEnv) =>

    val IsIn(i, _) = p

    if (!defined(ce)(i))
      throw new Exception("no class for instance")

    val its = insts(ce)(i)
    val qs = its.map {case Qual(_, q) => q}

    if (qs.exists(overlap(p)))
      throw new Exception("overlapping instance")

    val c = (super_(ce)(i), Qual(ps, p) :: its)

    modify(ce)(i)(c)
  }


  def exampleInsts : EnvTransformer =
    addPreludeClasses <:>
    addInst(List())(IsIn("Ord", tUnit)) <:>
    addInst(List())(IsIn("Ord", tChar)) <:>
    addInst(List())(IsIn("Ord", tInt))  <:>
    addInst(List(IsIn("Ord", TVar(Tyvar("a", Star))),
                 IsIn("Ord", TVar(Tyvar("b", Star))))) (
            IsIn("Ord", pair(TVar(Tyvar("a", Star))) (
                             TVar(Tyvar("b", Star)))))


  // 7.3 Entailment

  def bySuper (ce:ClassEnv) (p:Pred):List[Pred] = {
    val IsIn(i, t) = p
    val pss = super_(ce)(i).map {i =>
      bySuper(ce)(IsIn(i, t))
    }
    p :: pss.flatten
  }

  def byInst (ce: ClassEnv) (p:Pred): Option[List[Pred]] = {
    val IsIn(i, t) = p

    def tryInst(q:Qual[Pred]):Option[List[Pred]] = {
      q match { case Qual(ps, h) =>
        try {
         val u = matchPred(h)(p)
         Some (ps.map(predApply(u)) )
        } catch {
          case _ : Throwable => None
        }
      }
    }

    def msum(xs:List[Option[List[Pred]]]):Option[List[Pred]] = {
      xs match {
      case List() => None
      case None :: xs => msum(xs)
      case x :: _ => x
      }
    }

    msum(insts(ce)(i).map(tryInst))
  }

  def entail (ce:ClassEnv) (ps:List[Pred]) (p:Pred):Boolean = {
    ps.map (bySuper(ce)).exists{l => l.contains(p)} ||
    (byInst(ce)(p) match {
      case None => false
      case Some(qs) => qs.forall{p=>entail(ce)(ps)(p)}
    })
  }

  // 7.4 Context Reduction

  def inHnf (p:Pred):Boolean = {
    val IsIn(_, t) = p
    def hnf(t:Type_):Boolean = {
      t match {
        case TVar(_) => true
        case TCon(_) => false
        case TAp(t, _) => hnf(t)
        case TGen(_) => throw new Exception("context reduction on generic variable")
      }
    }
    hnf(t)
  }


  def toHnfs (ce:ClassEnv)(ps:List[Pred]):List[Pred] = {
    ps.map(toHnf(ce)).flatten
  }
  def toHnf (ce:ClassEnv)(p:Pred):List[Pred] = {
    if (inHnf(p)) List(p)
    else {
      byInst(ce)(p) match {
        case None => throw new Exception("context reduction")
        case Some(ps) => toHnfs(ce)(ps)
      }
    }
  }

  def simplify (ce:ClassEnv)(ps:List[Pred]):List[Pred] = {
    def loop(rs:List[Pred])(ps:List[Pred]):List[Pred] = {
      ps match {
        case List() => rs
        case p :: ps =>
          if (entail(ce)(rs ::: ps)(p)) loop(rs)(ps)
          else loop(p :: rs)(ps)
      }
    }
    
    loop(List())(ps)
  }

  def reduce (ce:ClassEnv)(ps:List[Pred]):List[Pred] = {
    simplify(ce)(toHnfs(ce)(ps))
  }

  def scEntail (ce:ClassEnv)(ps:List[Pred])(p:Pred):Boolean = {
    ps.map(bySuper(ce)).exists{p1=>p1.contains(p)} 
  }
    
}


// 8 Type Schemes
object Scheme {

  import Kind._
  import Type._
  import Pred._


  case class Forall(a:List[Kind], b:Qual[Type_])
  type Scheme = Forall

  def schemeApply (s:Subst.Subst) (sc:Scheme):Scheme = {
    val Forall(ks, qt) = sc
    Forall(ks, qualTypeApply(s)(qt))
  }

  def schemeTv (sc:Scheme):List[Tyvar] = {
    val Forall(_, qt) = sc
    qualTypeTv(qt)
  }

  var count = 0
  def quantify(vs:List[Tyvar]) (qt:Qual[Type_]):Scheme = {

    val vs_ = qualTypeTv(qt).filter{
        v => vs.contains(v)
      }

    val ks = vs_.map(tyvarKind)

    def newGen(v:Tyvar):(Tyvar,Type_) = {
      val t = TGen(count)
      count += 1
      (v, t)
    }

    val s = vs_.map(newGen)

    Forall(ks, qualTypeApply(s)(qt))
  }

  def toScheme (t:Type_) :Scheme = {
    Forall(List(), (Qual(List(), t)))
  }
}


// 9 Assumptions
object Assump {

  import Scheme._

  case class Assump(a:Id.Id,b:Scheme)

  def assumpApply (s:Subst.Subst) (as:Assump) : Assump = {
    val Assump(i, sc) = as
    Assump(i, schemeApply(s)(sc))
  }

  def assumpTv (as:Assump):List[Type.Tyvar] = {
    val Assump(_, sc) = as
    schemeTv(sc)
  }

  def assumpsApply (s:Subst.Subst) (ass:List[Assump]): List[Assump] = {
    Subst.listApply(assumpApply)(s)(ass)
  }

  def assumpsTv (ass:List[Assump]): List[Type.Tyvar] = {
    Subst.listTv(assumpTv)(ass)
  }

  def find (i:Id.Id) (ass:List[Assump]): Scheme = {

    ass.find { case Assump(i_, _) =>
      i == i_
    } match {
      case Some(Assump(_, sc)) => sc
      case None => throw new Exception("not found "+i)
    }
  }
}


// 10 A Type Inference Monad
object TIMonad {

  import Kind._
  import Type._
  import Subst._
  import Pred._
  import Scheme._

  case class Ti(var s:Subst, var n:Int)

  def runTI[A](f : Ti => A):A = {
    f(Ti(nullSubst, 0))
  }

  def getSubst(ti:Ti):Subst = ti.s


  def extSubst (ti : Ti) (u:Subst) {
    ti.s = u @@ ti.s
  }

  def unify (ti:Ti) (t1:Type_) (t2:Type_) {
    val s:Subst = getSubst(ti)
    val u = Unify.mgu(typeApply(s)(t1))(typeApply(s)(t2))
    extSubst(ti)(u)
  }

  def newTVar (ti:Ti)(k:Kind) : Type_ = {
    val v = Tyvar(Id.enumId(ti.n), k)
    ti.n += 1
    TVar(v)
  }


  def typeInst(ts:List[Type_])(t:Type_):Type_ = {
    t match {
      case TAp(l, r) => TAp(typeInst(ts)(l), typeInst(ts)(r))
      case TGen(n) => ts.apply(n)
      case t => t
    }
  }

  def listInst[A](inst: List[Type_] => A => A)(ts : List[Type_])(xs : List[A]): List[A] = {
    xs.map(inst(ts)) 
  }

  def predInst (ts: List[Type_]) (p: Pred):Pred = {
    val IsIn(c, t) = p
    IsIn(c, typeInst(ts)(t))
  }


  def qualTypeInst(ts:List[Type_])(q:Qual[Type_]):Qual[Type_] = {
    val Qual(ps, t) = q
    Qual(listInst(predInst)(ts)(ps), typeInst(ts)(t))
  }

  def freshInst (ti:Ti) (sc:Scheme) : Qual[Type_] = {
    val Forall(ks, qt) = sc
    val ts = ks.map(newTVar(ti))
    qualTypeInst(ts)(qt)
  }

}


// 11 Type Inference
object Infer {
  import Pred._
  import Assump._
  import TIMonad._

  type  Infer[E, T] = Ti => ClassEnv => List[Assump] => E => (List[Pred], T)
}

// 11.1 Literals
object Lit {
  import Kind._
  import Type._
  import Pred._
  import TIMonad._
  import Infer._

  sealed trait Literal
  case class LitInt(a:Long) extends Literal
  case class LitChar(a:Char) extends Literal
  case class LitRat(a:Double) extends Literal
  case class LitStr(a:String) extends Literal

  def tiLit (ti:Ti) (lit:Literal):(List[Pred], Type_) = {
    lit match {
      case LitChar(_) => (List(), tChar)
      case LitInt(_) =>
        val v = newTVar(ti)(Star)
        (List(IsIn("Num", v)), v)
      case LitStr(_) => (List(), tString)
      case LitRat(_) =>
        val v = newTVar(ti)(Star)
        (List(IsIn("Fractional", v)), v)
    }
  }
}


// 11.2 Patterns
object Pat {
  import Kind._
  import Type._
  import Pred._
  import Scheme._
  import TIMonad._
  import Infer._
  import Lit._

  trait Pat
  case class PVar(a:Id.Id) extends Pat
  case object PWildcard extends Pat
  case class PAs(a:Id.Id,b:Pat) extends Pat
  case class PLit(a:Literal) extends Pat
  case class PNpk(a:Id.Id,b:Long) extends Pat
  case class PCon(a:Assump.Assump,b:List[Pat]) extends Pat

  def tiPat (ti:Ti) (pat:Pat):(List[Pred], List[Assump.Assump], Type_) = {
    pat match {
      case PVar(i) =>
        val t = newTVar(ti)(Star)
        (List(), List(Assump.Assump(i, toScheme(t))), t)
      case PWildcard => (List(), List(), newTVar(ti)(Star))
      case PAs(i, pat) =>
        val (ps, as_, t) = tiPat(ti)(pat)
        (ps, Assump.Assump(i, toScheme(t)) :: as_, t)
      case PLit(l) =>
        val (ps, t) = tiLit(ti)(l)
        (ps, List(), t)
      case PNpk(i, k) =>
        val t = newTVar(ti)(Star)
        (List(IsIn("Integral", t)), List(Assump.Assump(i, toScheme(t))), t)
      case PCon(Assump.Assump(i, sc), pats) =>
        val (ps, as_, ts) = tiPats(ti)(pats)
        val t_ = newTVar(ti)(Star)
        val Qual(qs, t) = freshInst(ti)(sc)
        unify(ti)(t)(ts.foldRight(t_){(a,b)=>fn(a)(b)})
        (ps ::: qs, as_, t_)
    }
  }

  def tiPats (ti:Ti) (pats:List[Pat]):(List[Pred], List[Assump.Assump], List[Type_]) = {
    val (pss, ass, ts) = Pre.split3 (pats.map(tiPat(ti)))
    (pss.flatten, ass.flatten, ts)
  }
}


// 11.3 Expressions
// 11.4 Alternatives
// 11.5 From Types to Type Schemes
// 11.6 Binding Groups
object TIMain {
  import Kind._
  import Type._
  import Pred._
  import Subst._
  import TIMonad._
  import Infer._
  import Lit._
  import Pat._
  import Scheme._
  //import Assump._

  type Ambiguity = (Tyvar, List[Pred])

  def ambiguities (vs:List[Tyvar]) (ps:List[Pred]) : List[Ambiguity] = {
    val vs_ = Pre.diff(predsTv(ps))(vs)
    vs_.map{ v =>
      (v, ps.filter{ p =>
        predTv(p).contains(v) 
      })
    } 
  }

  val numClasses : List[Id.Id] = List(
    "Num", "Integral", "Floating", "Fractional", "Real", "RealFloat",
    "RealFrac")

  val stdClasses : List[Id.Id] = List(
    "Eq", "Ord", "Show", "Read", "Bounded", "Enum", "Ix", "Functor", "Monad",
    "MonadPlus") ::: numClasses

  def candidates (ce:ClassEnv) (amb : Ambiguity): List[Type_] = {
    val (v, qs) = amb
    val is = qs.map {case IsIn(i, _) => i}
    val ts = qs.map {case IsIn(_, t) => t}
    if (ts.forall{ t => t == TVar(v)} &&
      is.exists{ i => numClasses.contains(i) } &&
      is.forall{ i => stdClasses.contains(i) }) {
      ce.defaults.filter{ t_ =>
          is.map { i => IsIn(i, t_)}.forall(entail(ce)(List()))
      }
    } else List()
  }

  def withDefaults[A](f:List[Ambiguity] => List[Type_] => A)
    (ce:ClassEnv) (vs:List[Tyvar]) (ps:List[Pred]):A = {
    val vps = ambiguities(vs)(ps)
    val tss = vps.map(candidates(ce))
    if (tss.exists(Pre.isEmpty))
      throw new Exception("cannot resolve ambiguity")
    f(vps)(tss.map{_.head})
  }

  def defaultedPreds (ce:ClassEnv) (vs:List[Tyvar]) (ps:List[Pred]):List[Pred] = {
    withDefaults {vps => ts => vps.map{_._2}.flatten} (ce)(vs)(ps)
  }

  def defaultSubst (ce:ClassEnv) (vs:List[Tyvar]) (ps:List[Pred]): Subst = {
    withDefaults {vps => ts => vps.map{_._1}.zip(ts)} (ce)(vs)(ps)
  }

  def split (ce:ClassEnv) (fs:List[Tyvar]) (gs:List[Tyvar])
    (ps:List[Pred]): (List[Pred], List[Pred]) = {
    val ps_ = reduce(ce)(ps)
    val (ds, rs) = {
      ps_.partition { p =>
        predTv(p).forall { fs.contains(_) }
      }
    }
    val rs_ = defaultedPreds(ce)(fs ::: gs)(rs)
    (ds, Pre.diff(rs)(rs_))
  }


  sealed trait Expr
  case class Var(a:Id.Id) extends Expr
  case class Lit_(a:Literal) extends Expr
  case class Const(a:Assump.Assump) extends Expr
  case class Ap(a:Expr, b: Expr) extends Expr
  case class Let(a:BindGroup, b:Expr) extends Expr
  //case class Lam of alt) extends Expr
  //case class If of expr * expr * expr) extends Expr
  //case class Case of expr * (Pat * Expr) list) extends Expr

  type Alt = (List[Pat], Expr)
  type Expl = (Id.Id, Scheme, List[Alt])
  type Impl = (Id.Id, List[Alt])
  type BindGroup = (List[Expl], List[List[Impl]])


  def restricted (bs : List[Impl]):Boolean = { 
    bs.exists{case (i, alts) =>
      alts.exists { alt =>
        Pre.isEmpty (alt._1)
      }
    }
  }

  //type  Infer[E, T] = Ti => ClassEnv => List[Assump] => E => (List[Pred], T)

  def tiSeq[BG] (f : Infer[BG, List[Assump.Assump]]) : Infer[List[BG], List[Assump.Assump]] = {
    def f2(ti:Ti)(ce:ClassEnv)(as_ :List[Assump.Assump])(ls:List[BG]):(List[Pred],List[Assump.Assump]) = {
      ls match {
        case List() => (List(), List())
        case bs :: bss =>
          val (ps, as_2) = f(ti)(ce)(as_)(bs)
          val (qs, as__) = tiSeq(f)(ti)(ce)(as_2 ::: as_)(bss)
          (ps ::: qs, as__ ::: as_2)
      }
    }
    f2
  }

  def tiExpr (ti:Ti)(ce:ClassEnv)(as_ :List[Assump.Assump])(expr: Expr): (List[Pred], Type_) =
    expr match {
      case Var (i) =>
        val sc = Assump.find(i)(as_)
        val Qual(ps, t) = freshInst(ti)(sc)
        (ps, t)
      case Const(Assump.Assump(_, sc)) =>
        val Qual(ps, t) = freshInst(ti)(sc)
        (ps, t)
      case Lit_(l) => tiLit(ti)(l)
      case Ap(e, f) =>
        val (ps, te) = tiExpr(ti)(ce)(as_)(e)
        val (qs, tf) = tiExpr(ti)(ce)(as_)(f)
        val t = newTVar(ti)(Star)
        unify(ti)(fn(tf)(t))(te)
        (ps ::: qs, t)
      case Let(bg, e) =>

        val (ps, as2) = tiBindGroup(ti)(ce)(as_)(bg)
        val (qs, t) = tiExpr(ti)(ce)(as2 ::: as_)(e)
        (ps ::: qs, t)
      //case Lam(alt) => tiAlt(ti)(ce)(as_)alt
      /*case If(e, e1, e2) =>
        def (ps,t) = tiExpr(ti)(ce)(as_)e in
        unify(ti)t tBool;
        def (ps1,t1) = tiExpr(ti)(ce)(as_)e1 in
        def (ps2,t2) = tiExpr(ti)(ce)(as_)e2 in
        unify(ti)t1 t2;
        (ps @ ps1 @ ps2, t1)*/
      /*case Case(e, branches) =>
        def (ps, t) = tiExpr(ti)(ce)(as_)e in
        def v = newTVar Star in
        def tiBr (pat, f) =
          def (ps, _as',t') = tiPat pat in
          unify t t';
          def (qs, t'') = tiExpr (ce)(_as' @ _as) f in
          unify v t'';
          (ps @ qs)
        in
        def pss = mapM tiBr branches in
        (ps @ concat pss, v)
      */
    }

//type  Infer[E, T] = Ti => ClassEnv => List[Assump.Assump] => E => (List[Pred], T)
  def tiAlt : Infer[Alt, Type_] = {

    def f(ti:Ti)(ce:ClassEnv)(as_ :List[Assump.Assump])(alt:Alt):
      (List[Pred], Type_) = {
      val (pats, e) = alt
      val (ps, as1, ts) = tiPats(ti)(pats)
      val (qs, t) = tiExpr(ti)(ce)(as1 ::: as_)(e)
      (ps ::: qs, ts.foldRight(t){(a,b)=>fn(a)(b)})
    }
    f
  }
//type  Infer[E, T] = Ti => ClassEnv => List[Assump.Assump] => E => (List[Pred], T)
  def tiAlts(ti:Ti)(ce:ClassEnv)(as_ :List[Assump.Assump])(alts:List[Alt])(t:Type_):
    List[Pred] = {

    val (ps, ts) = alts.map{tiAlt(ti)(ce)(as_)}.unzip
    ts.foreach{ unify(ti)(t) }
    ps.flatten
  }
//type  Infer[E, T] = Ti => ClassEnv => List[Assump.Assump] => E => (List[Pred], T)
  def tiExpl (ti:Ti)(ce:ClassEnv)(as_ :List[Assump.Assump])
    (expl : Expl):List[Pred] = {
  
    val (i, sc, alts) = expl
    val Qual(qs, t) = freshInst(ti)(sc)
    val ps = tiAlts(ti)(ce)(as_)(alts)(t)
    val s = getSubst(ti)
    val qs2 = predsApply(s)(qs)
    val t2 = typeApply(s)(t)
    val fs = Assump.assumpsTv(Assump.assumpsApply(s)(as_))
    val gs = Pre.diff(typeTv(t2))(fs)
    val sc2 = quantify(gs)(Qual(qs2, t2))
    val ps_ = predsApply(s)(ps).filter{ p => !entail(ce)(qs2)(p) }
    val (ds, rs) = split(ce)(fs)(gs)(ps_)
    if (sc != sc2) throw new Exception("signature too general")
    if (!Pre.isEmpty(rs)) throw new Exception("context too weak")
    ds
  }

//type  Infer[E, T] = Ti => ClassEnv => List[Assump.Assump] => E => (List[Pred], T)
  def tiImpls : Infer[List[Impl], List[Assump.Assump]] = {
  
    def f(ti:Ti)(ce:ClassEnv)(as_ :List[Assump.Assump])(bs:List[Impl]):
      (List[Pred], List[Assump.Assump]) = {
      val (is,ts2,gs,ds,rs) = {
        val (is, ps_,ts2:List[Type_],fs:List[Tyvar]) = {
          val ts = bs.map { _ => newTVar(ti)(Star) }
          val (is, altss) = bs.unzip
          val scs = ts.map(toScheme)
          val as1 = is.zip(scs).map{case (i,sc) => Assump.Assump(i, sc)}  ::: as_
          val pss = altss.zip(ts).map{case(a,b)=>tiAlts(ti)(ce)(as1)(a)(b)} 
          val s = getSubst(ti)
          val ps_ = pss.flatten.map(predApply(s))
          val ts2:List[Type_] = ts.map(typeApply(s))
          val fs = Assump.assumpsTv(Assump.assumpsApply(s)(as_))
          (is, ps_,ts2,fs)
        }
        val vss:List[List[Tyvar]] = ts2.map(typeTv)
        val vss2:List[Tyvar] = Pre.fold_left1[List[Tyvar]]{
          case(a,b)=>Pre.union(a)(b)
        }(vss)
        val gs = Pre.diff(vss2)(fs)
        val (ds, rs) = split(ce)(fs)(Pre.fold_left1[List[Tyvar]]{case(a,b)=>Pre.intersect(a)(b)}(vss))(ps_)
        (is, ts2,gs,ds,rs)
      }

      if (restricted(bs)) {
        val gs2 = Pre.diff(gs)(predsTv(rs))
        val scs2 = ts2.map{ t => quantify(gs2)(Qual(List(), t))}
        (ds ::: rs, is.zip(scs2).map { case (i, sc) => Assump.Assump(i, sc)} )
      } else {
        val scs1 = ts2.map{ t => quantify(gs)(Qual(rs, t)) }
        (ds, is.zip(scs1).map { case(i, sc) => Assump.Assump(i, sc)})
      }
    }
    f
  }

  def tiBindGroup : Infer[BindGroup, List[Assump.Assump]] = {
    def f(ti:Ti)(ce:ClassEnv)(as_ :List[Assump.Assump])(bg:BindGroup):
      (List[Pred],List[Assump.Assump]) = {
      val (es, iss) = bg
      val as1 = es.map {case (v, sc, _) => Assump.Assump(v, sc)}
      val (ps, as2) = tiSeq(tiImpls)(ti)(ce)(as1 ::: as_)(iss)
      val qss = es.map{tiExpl(ti)(ce)(as2 ::: as1 ::: as_)}
      (ps ::: (qss.flatten), as2 ::: as1)
    }
    f
  }
  type Program = List[BindGroup]

  def tiProgram(ce:ClassEnv)(as_ :List[Assump.Assump])(bgs : Program):
    List[Assump.Assump] = {
    runTI { ti =>
      val (ps, as2) = tiSeq(tiBindGroup)(ti)(ce)(as_)(bgs)
      val s = getSubst(ti)
      val rs = reduce(ce)(predsApply(s)(ps))
      val s2 = defaultSubst(ce)(List())(rs)
      Assump.assumpsApply (s2 @@ s) (as2)
    }
  }
}

