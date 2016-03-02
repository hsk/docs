object Infer extends App {

  sealed trait Exp
  case class EVar(a: String) extends Exp
  case class EInt(a: Int) extends Exp
  case class EBool(a: Boolean) extends Exp
  case class EApp(a: Exp, b: Exp) extends Exp
  case class EAbs(a: String, b: Exp) extends Exp
  case class ELet(a: String, b: Exp, c: Exp) extends Exp

  sealed trait Type
  case object TInt extends Type
  case object TBool extends Type
  case class TVar(a: String) extends Type
  case class TFun(a:Type, b: Type) extends Type

  type Subst = Map[String, Type]
  type Assumps = Map[String, Type]

  case class TypeError(s: String) extends Exception(s)

  def show(t: Type): String = t.toString
  def show(e: Exp): String = e.toString

  def ftv_type(t: Type): Set[String] =
    t match {
      case TVar(n)      => Set(n)
      case TInt         => Set()
      case TBool        => Set()
      case TFun(t1, t2) => ftv_type(t1).union(ftv_type(t2))
    }

  def apply_type(s: Subst, t: Type): Type =
    t match {
      case TVar(n)      => s.getOrElse(n, TVar(n))
      case TFun(t1, t2) => TFun(apply_type(s, t1), apply_type(s, t2))
      case t            => t
    }

  def apply_assumps(s: Subst, assumps: Assumps): Assumps =
    assumps.map {
      case (k, v) => (k, apply_type(s, v))
    }

  val nullSubst = Map[String, Type]()

  def composeSubst(s1: Subst, s2: Subst): Subst =
    s2.map {
      case (x, v) =>
        (x, apply_type(s1, v))
    } ++ s1

  var tiSupply = 0

  def newTVar(prefix: String): Type = {
    val s = tiSupply
    tiSupply = s + 1
    TVar(prefix + s)
  }

  def varBind(u: String, t: Type): Subst = 
    if (t == TVar(u)) nullSubst
    else if (ftv_type(t).contains(u))
      throw TypeError("occurs check fails: " + u + " vs. " + show(t))
    else Map(u -> t)

  def mgu(t1: Type, t2: Type): Subst = {
    (t1,t2) match {
      case (TFun(l, r),TFun(l2, r2)) =>
        val s1 = mgu(l, l2)
        val s2 = mgu(apply_type(s1, r), apply_type(s1, r2))
        composeSubst(s1, s2)
      case (TVar(u), t) => varBind(u, t)
      case (t, TVar(u)) => varBind(u, t)
      case (TInt, TInt) => nullSubst
      case (TBool,TBool) => nullSubst
      case (t1,t2) =>
        throw TypeError("types do not unify: " + show(t1) + " vs. " + show (t2))
    }
  }

  // Main type inference function
  def ti(env: Assumps, e: Exp): (Subst, Type) = {
    e match {
    case EVar(n) => 
      if (env.contains(n))
        (nullSubst, env(n))
      else
        throw TypeError("unbound variable: " + n)
    case EInt(_)  => (nullSubst, TInt)
    case EBool(_) => (nullSubst, TBool)
    case EAbs(n, e) =>
      val tv = newTVar("'a")
      val env2 = env + (n -> tv)
      val (s1, t1) = ti(env2, e)
      (s1, TFun(apply_type(s1, tv), t1))
    case exp @ EApp(e1, e2) =>
      try {
        val tv = newTVar("'a")
        val (s1, t1) = ti(env, e1)
        val (s2, t2) = ti(apply_assumps(s1, env), e2)
        val s3 = mgu(apply_type(s2, t1), TFun(t2, tv))
        (composeSubst(s3,composeSubst(s2,s1)), apply_type(s3, tv))
      } catch {
        case TypeError(e) => throw TypeError(e + "\n in " + show(exp))
      }
    case ELet(x, e1, e2) =>
      val (s1, t1) = ti(env, e1)
      val env2 = env + (x -> t1)
      val (s2, t2) = ti(apply_assumps(s1, env2), e2)
      (composeSubst(s1, s2), t2)
    }
  }

  def type_inference(env:Map[String,Type], e: Exp):Type = {
    val (s, t) = ti(env, e)
    apply_type(s, t)
  }

  // Tests
  def test(e: Exp, et: Type) {
    try {
      val t = type_inference(Map(), e)
      if(t!=et)println(show(e) + " :: " + show(t) + "\n")
      assert(t == et)
    } catch {
      case TypeError(err) =>
        println(show(e) + "\n " + err + "\n")
        assert(false)
    }
  }

  def testError(e:Exp) {
    try {
      val t = type_inference(Map(), e)
      println(show(e) + " :: " + show(t) + "\n")

      assert(false)
    } catch {
      case TypeError(err) =>
    }
  }

  // Main Program
  test(ELet("id", EAbs("x", EVar("x")),
    EVar("id")),
    TFun(TVar("'a0"), TVar("'a0")))

  test(ELet("id", EAbs("x", EVar("x")),
    EApp(EVar("id"), EInt(1))),
    TInt)

  test(ELet("id", EAbs("x", ELet("y", EVar("x"), EVar("y"))),
   EApp(EVar("id"), EInt(1))),
    TInt)

  testError(ELet("id", EAbs("x", ELet("y", EVar("x"), EVar("y"))),
   EApp(EApp(EVar("id"), EVar("id")), EInt(2))))

  testError(ELet("id", EAbs("x", EApp(EVar("x"), EVar("x"))),
   EVar("id")))

  test(EAbs("m", ELet("y", EVar("m"),
        ELet("x", EApp(EVar("y"), EBool(true)),
              EVar("x")))),
    TFun(TFun(TBool,TVar("'a11")),TVar("'a11")))

  testError(EApp(EInt(2), EInt(2)))

}

