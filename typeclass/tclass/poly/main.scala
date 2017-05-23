package main
object main extends App {

  sealed trait E
  case class EVar(a: String) extends E
  case class EInt(a: Int) extends E
  case class EBool(a: Boolean) extends E
  case class EApp(a: E, b: E) extends E
  case class EAbs(a: String, b: E) extends E
  case class ELet(a: String, b: E, c: E) extends E

  sealed trait T
  case class TVar(a: String) extends T
  case object TInt extends T
  case object TBool extends T
  case class TFun(a:T, b: T) extends T

  case class Scheme(a:List[String], t:T)

  type Subst = Map[String, T]
  type Assumps = Map[String, Scheme]

  case class TypeError(s: String) extends Exception(s)

  def show_t(t: T): String = t.toString
  def show_e(e: E): String = e.toString

  val nullSubst = Map[String, T]()
  var subst = nullSubst

  var count = 0

  def new_tvar(prefix: String): T = {
    val s = count
    count = s + 1
    TVar(prefix + s)
  }

  def ftv_t(t: T): Set[String] =
    t match {
      case TVar(n)      => Set(n)
      case TInt         => Set()
      case TBool        => Set()
      case TFun(t1, t2) => ftv_t(t1).union(ftv_t(t2))
    }

  def ftv_scheme(scheme: Scheme): Set[String] =
    scheme match {
      case Scheme(vars, t) => ftv_t(t).diff(vars.toSet)
    }

  def ftv_assumps(assumps: Assumps): Set[String] =
    assumps.values.toList.map(ftv_scheme).foldRight(Set[String]()){
      case (x, s) => s.union(x)
    }

  def apply_subst(s:Subst, t: T): T =
    t match {
      case TVar(n)      => s.getOrElse(n, TVar(n))
      case TFun(t1, t2) => TFun(apply_subst(s, t1), apply_subst(s, t2))
      case t            => t
    }

  def apply_t(t: T): T = apply_subst(subst, t)

  def apply_scheme(scheme: Scheme): Scheme = {
    scheme match {
      case Scheme(vars, t) =>
        Scheme(vars, apply_subst(vars.foldRight(subst){ case(x, s) => s - x }, t))
    }
  }

  def apply_assumps(assumps: Assumps): Assumps =
    assumps.map {
      case (k, v) => (k, apply_scheme(v))
    }

  def generalize(env: Assumps, t: T): Scheme = {
    val vars = ftv_t(t).diff(ftv_assumps(env)).toList
    Scheme(vars, t)
  }

  def instantiate(scheme: Scheme): T = {
    scheme match {
      case Scheme(vars, t) =>
        val s = vars.foldLeft(Map[String, T]()) {
          case (subst, k) => subst + (k -> new_tvar("a"))
        }
        apply_subst(s, t)
    }
  }

  def var_bind(u: String, t: T) {
    if (t != TVar(u)) {
      if (ftv_t(t).contains(u))
        throw TypeError("occurs check fails: " + u + " vs. " + show_t(t))
      subst = subst + (u -> t)
    }
  }

  def mgu(t1: T, t2: T) {
    (t1,t2) match {
      case (TFun(l, r),TFun(l2, r2)) =>
        mgu(l, l2)
        mgu(apply_t(r), apply_t(r2))
      case (TVar(u), t) => var_bind(u, t)
      case (t, TVar(u)) => var_bind(u, t)
      case (TInt, TInt) =>
      case (TBool, TBool) =>
      case (t1,t2) =>
        throw TypeError("types do not unify: " + show_t(t1) + " vs. " + show_t(t2))
    }
  }

  def ti(env: Assumps, e: E): T = {
    e match {
    case EVar(n) =>
      if (!env.contains(n))
        throw TypeError("unbound variable: " + n)
      instantiate(env(n))
    case EInt(_)  => TInt
    case EBool(_) => TBool
    case EAbs(n, e) =>
      val tv = new_tvar("a")
      val t1 = ti(env + (n -> Scheme(List(), tv)), e)
      TFun(apply_t(tv), t1)
    case EApp(e1, e2) =>
      try {
        val tv = new_tvar("a")
        val t1 = ti(env, e1)
        val t2 = ti(apply_assumps(env), e2)
        mgu(apply_t(t1), TFun(t2, tv))
        apply_t(tv)
      } catch {
        case TypeError(msg) => throw TypeError(msg + "\n in " + show_e(e))
      }
    case ELet(x, e1, e2) =>
      val t1 = ti(env, e1)
      val t11 = generalize(apply_assumps(env), t1)
      ti(apply_assumps(env + (x -> t11)), e2)
    }
  }

  def type_inference(env:Map[String,Scheme], e: E):T = {
    subst = nullSubst
    val t = ti(env, e)
    apply_t(t)
  }

  def test(e: E, et: T) {
    try {
      val t = type_inference(Map(), e)
      assert(t == et)
    } catch {
      case TypeError(err) =>
        println(show_e(e) + "\n " + err + "\n")
        assert(false)
    }
  }

  def test_error(e:E) {
    try {
      type_inference(Map(), e)
      assert(false)
    } catch {
      case TypeError(err) =>
    }
  }

  test(ELet("id", EAbs("x", EVar("x")),
    EVar("id")),
    TFun(TVar("a1"), TVar("a1")))

  test(ELet("id", EAbs("x", EVar("x")),
    EApp(EVar("id"), EVar("id"))),
    TFun(TVar("a5"), TVar("a5")))

  test(ELet("id", EAbs("x", ELet("y", EVar("x"), EVar("y"))),
   EApp(EVar("id"), EVar("id"))),
    TFun(TVar("a9"), TVar("a9")))

  test(ELet("id", EAbs("x", ELet("y", EVar("x"), EVar("y"))),
   EApp(EApp(EVar("id"), EVar("id")), EInt(2))),
    TInt)

  test_error(ELet("id", EAbs("x", EApp(EVar("x"), EVar("x"))),
   EVar("id")))

  test(EAbs("m", ELet("y", EVar("m"),
        ELet("x", EApp(EVar("y"), EBool(true)),
              EVar("x")))),
    TFun(TFun(TBool,TVar("a18")),TVar("a18")))

  test_error(EApp(EInt(2), EInt(2)))

}
