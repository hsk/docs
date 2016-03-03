object Infer extends App {

  sealed trait E
  case class EVar(a: String) extends E
  case class EInt(a: Int) extends E
  case class EBool(a: Boolean) extends E
  case class EApp(a: E, b: E) extends E
  case class EAbs(a: String, b: E) extends E
  case class ELet(a: String, b: E, c: E) extends E

  case class ERecord(map:Map[String,E]) extends E
  case class ERecordGet(e:E,f:String) extends E
  case class EType(name:String, prm:List[String], t:T, e:E) extends E

  case class EPlaceHolder(e:String, t:T) extends E
  case class EClass(name:String,p:String,members:Map[String,T],e:E) extends E
  case class EInst(name:String,p:T, members:Map[String, E],e:E) extends E

  sealed trait T
  case object TInt extends T
  case object TBool extends T
  case class TVar(a: String) extends T
  case class TFun(a:T, b: T) extends T
  case class TRecord(t:Map[String,T]) extends T
  case class TCon(a: String, ts:List[T]) extends T

  sealed trait Scheme
  case class Mono(t:T) extends Scheme
  case object Over extends Scheme

  type Subst = Map[String, T]
  type Assumps = Map[String, Scheme]

  case class TClass(name:String, members:Map[String,T],impls:Map[T,String])
  type CEnv = Map[String, TClass]

  case class TypeError(s: String) extends Exception(s)

  def show_t(t: T): String = t.toString
  def show_e(e: E): String = e.toString

  val nullSubst = Map[String, T]()
  var subst = nullSubst

  var counter = 0

  def new_tvar(prefix: String): T = {
    val s = counter
    counter = s + 1
    TVar(prefix + s)
  }

  def ftv_t(t: T): Set[String] =
    t match {
      case TVar(n)      => Set(n)
      case TInt         => Set()
      case TBool        => Set()
      case TFun(t1, t2) => ftv_t(t1).union(ftv_t(t2))
      case TRecord(map) =>
        map.foldLeft(Set[String]()) { case (tvs, (k,t)) => tvs.union(ftv_t(t)) }      
    }

  def apply_subst(s:Subst, t:T):T = {
    t match {
      case TVar(a) =>
        s.get(a) match {
          case Some(a) => apply_subst(s, a)
          case None => t
        }
      case TFun(t1, t2) => TFun(apply_subst(s, t1), apply_subst(s, t2))
      case TRecord(map) => TRecord(map.map{case(k,t)=> (k,apply_subst(s, t)) })
      case t            => t
    }
  }

  def apply_t(t: T): T = apply_subst(subst, t)


  def instantiate(cenv:CEnv, env: Assumps, e:E, n:String):(CEnv, E, T) = {
    env(n) match {
      case Mono(t) => (cenv, e, t)
      case Over => // オーバーロードされた型はプレースホルダにする
        val t = new_tvar("'dictOver")
        (cenv, EPlaceHolder(n, t), t)
    }
  }

  def mgu(t1: T, t2: T) {
    (apply_t(t1), apply_t(t2)) match {
      case (TFun(l, r),TFun(l2, r2)) =>
        mgu(l, l2)
        mgu(r, r2)
      case (TVar(u), t) =>
        if (t != TVar(u)) {
          if (ftv_t(t).contains(u))
            throw TypeError("occurs check fails: " + u + " vs. " + show_t(t))
          else subst = subst + (u -> t)
        }
      case (t, u @ TVar(_)) => mgu(u, t)
      case (TInt, TInt) =>
      case (TBool,TBool) =>
      case (t@TRecord(map1), u@TRecord(map2)) =>
        if (map1.keys != map2.keys) 
          throw new TypeError("cannot unify " + show_t(t) + " with " + show_t(u))
        map1.foreach{case(k,t)=>
          mgu(map2(k),t)
        }
      case (t1,t2) =>
        throw TypeError("types do not unify: " + show_t(t1) + " vs. " + show_t(t2))
    }
  }

  def ti(cenv:CEnv, env: Assumps, e: E): (CEnv, E, T) = {
    e match {
    case EVar(n) =>
      if (!env.contains(n))
        throw TypeError("unbound variable: " + n)
      instantiate(cenv, env, e, n)      
    case EInt(_)  => (cenv, e, TInt)
    case EBool(_) => (cenv, e, TBool)
    case EAbs(n, e1) =>
      val t = new_tvar("'abs")
      val (cenv1, e1_, t1) = ti(cenv, env + (n -> Mono(t)), e1)
      (cenv1, EAbs(n, e1), TFun(t, t1))
    case EApp(e1, e2) =>
      try {
        val t = new_tvar("'app")
        val (cenv1, e1_, t1) = ti(cenv, env, e1)
        val (cenv2, e2_, t2) = ti(cenv1, env, e2)
        mgu(t1, TFun(t2, t))
        (cenv2, EApp(e1_, e2_), t)
      } catch {
        case TypeError(msg) => throw TypeError(msg + "\n in " + show_e(e))
      }

    case ELet(x, e1, e2) =>
      val (cenv1, e1_, t1) = ti(cenv, env, e1)
      val (cenv2, e2_, t2) = ti(cenv1, env + (x -> Mono(t1)), e2)
      (cenv2, ELet(x,e1_,e2_), t2)
    
    case EClass(name,p1,members,e) =>
      val cenv2 = cenv + (name -> TClass(p1, members, Map()))
      val env2 = members.foldLeft(env) {
        case(env, (name1, t)) =>
          env + (name1 -> Over)
      }
      val (cenv_, r, t) = ti(cenv2, env2, e)
      (cenv_, EType(name, List(p1), TRecord(members), r), t)

    case EInst(name, t1, members, e) =>
      cenv(name) match {
      case TClass(p, tmap, impls) =>
        val TVar(dict) = new_tvar("dict_") 
        val cenv2 = cenv + (name -> TClass(p, tmap, impls+(t1->dict)))
        if (tmap.keys != members.keys)
          throw TypeError("instance member error "+name+" "+show_t(t1))
        members.foreach{case (k, e) =>
          val (cenv_, r1, instt1) = ti(cenv2, env, e)
          val instt = apply_subst(Map(p->t1), tmap(k))
          mgu(instt, instt1)
        }
        val (cenv3, r2, t) = ti(cenv2, env, e)
        (cenv3, ELet(dict, ERecord(members), r2), t)
      }
    case e =>
      throw TypeError("invalid expression "+e)
    }
  }

  def plane(cenv:CEnv, e: E): E = {
    e match {
    case EVar(n) => e
    case EInt(_)  => e
    case EBool(_) => e
    case EAbs(n, e1) => EAbs(n, plane(cenv, e1))
    case EApp(e1, e2) => EApp(plane(cenv, e1),plane(cenv, e2))
    case ELet(x, e1, e2) => ELet(x, plane(cenv, e1), plane(cenv, e2))
    case ERecord(map) => ERecord(map.map{case(k,e)=>(k,plane(cenv, e))})
    case ERecordGet(e, f) => ERecordGet(plane(cenv, e), f)
    case EType(name, prm, t, e) => EType(name,prm,t, plane(cenv, e))
    case EPlaceHolder(n, t) =>
      cenv.find{case(klass,tclass)=>tclass.members.contains(n)} match {
        case Some((klass,TClass(name:String, members:Map[String,T],impls:Map[T,String]))) =>
          val tv = new_tvar("some")
          val t1 = apply_subst(Map(name->tv), members(n))
          mgu(t, t1)
          ERecordGet(EVar(impls(apply_t(tv))), n)
        case None => EVar(n)
      }
    case e =>
      throw TypeError("invalid expression "+e)
    }
  }

  def type_inference(env:Assumps, e: E):(CEnv, E, T) = {
    subst = nullSubst
    val (cenv, e_, t) = ti(Map(), env, e)
    (cenv, plane(cenv, e_), apply_t(t))
  }

  def test(e: E, et: T) {
    try {
      val (_,e_,t) = type_inference(Map(), e)
      if(t!=et)println(show_e(e) + " :: " + show_t(t) + "\n")
      assert(t == et)
    } catch {
      case TypeError(err) =>
        println(show_e(e) + "\n " + err + "\n")
        assert(false)
    }
  }

  def test_error(e:E) {
    try {
      val (_,e_,t) = type_inference(Map(), e)
      println(show_e(e) + " :: " + show_t(t) + "\n")
      assert(false)
    } catch {
      case TypeError(err) =>
    }
  }

  def test2(env:Map[String,Scheme], e: E, ee: E):Boolean = {
    try {
      val (cenv_, e_,t) = type_inference(env, e)
      println(cenv_)
      println(e_)
      e_ == ee
    } catch {
      case TypeError(err) =>
        println(show_e(e) + "\n " + err + "\n")
        false
    }
  }

  def test_error2(e:E) {
    try {
      val (_, e_,t) = type_inference(Map(), e)
      println(show_e(e) + " :: " + show_t(t) + "\n")
      assert(false)
    } catch {
      case TypeError(err) =>
    }
  }

  test(ELet("id", EAbs("x", EVar("x")),
    EVar("id")),
    TFun(TVar("'abs0"), TVar("'abs0")))

  test(ELet("id", EAbs("x", EVar("x")),
    EApp(EVar("id"), EInt(1))),
    TInt)

  test(ELet("id", EAbs("x", ELet("y", EVar("x"), EVar("y"))),
    EApp(EVar("id"), EInt(1))),
    TInt)

  test_error(ELet("id", EAbs("x", ELet("y", EVar("x"), EVar("y"))),
    EApp(EApp(EVar("id"), EVar("id")), EInt(2))))

  test_error(ELet("id", EAbs("x", EApp(EVar("x"), EVar("x"))),
    EVar("id")))

  test(EAbs("m", ELet("y", EVar("m"),
        ELet("x", EApp(EVar("y"), EBool(true)),
              EVar("x")))),
    TFun(TFun(TBool,TVar("'app11")),TVar("'app11")))

  test_error(EApp(EInt(2), EInt(2)))


  test(
    EClass("Num","a",Map("add"->TFun(TVar("a"),TFun(TVar("a"),TVar("a")))),
    EInt(1)),
    TInt)

  assert(test2(
    Map("+"->Mono(TFun(TInt,TFun(TInt,TInt)))),

    EClass("Num","a",Map("add"->TFun(TVar("a"), TFun(TVar("a"), TVar("a")))),
    EInst("Num",TInt,Map("add"->EAbs("x",EAbs("y",EApp(EApp(EVar("+"),EVar("x")),EVar("y"))))),
    EApp(EApp(EVar("add"),EInt(1)),EInt(2)))),

    EType("Num",List("a"),TRecord(Map(
      "add" -> TFun(TVar("a"),TFun(TVar("a"),TVar("a"))))),
    ELet("dict_13",ERecord(Map(
      "add" -> EAbs("x",EAbs("y",EApp(EApp(EVar("+"),EVar("x")),EVar("y")))))),
    EApp(EApp(ERecordGet(EVar("dict_13"),"add"),EInt(1)),EInt(2))))))
}

