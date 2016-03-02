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

  case class EPlaceHolder(e:E, t:T) extends E
  case class EClass(name:String,p:String,members:Map[String,T],e:E) extends E
  case class EInst(name:String,p:T, members:Map[String, E],e:E) extends E

  sealed trait T
  case object TInt extends T
  case object TBool extends T
  case class TVar(a: String) extends T
  case class TFun(a:T, b: T) extends T
  case class TRecord(t:Map[String,T]) extends T
  case class TCon(a: String, ts:List[T]) extends T

  type Subst = Map[String, T]
  sealed trait Scheme
  case class Mono(t:T) extends Scheme
  case object Over extends Scheme
  type Assumps = Map[String, Scheme]

  var subst = nullSubst

  case class TypeError(s: String) extends Exception(s)

  def show_t(t: T): String = t.toString
  def show_e(e: E): String = e.toString

  def ftv_type(t: T): Set[String] =
    t match {
      case TVar(n)      => Set(n)
      case TInt         => Set()
      case TBool        => Set()
      case TFun(t1, t2) => ftv_type(t1).union(ftv_type(t2))
      case TRecord(map) =>
        map.foldLeft(Set[String]()) { case (tvs, (k,t)) => tvs.union(ftv_type(t)) }      
    }

  def ftv_scheme(sc:Scheme): Set[String] =
    sc match {
      case Mono(t) => ftv_type(t)
      case Over => Set() 
    }

  def apply_type(t: T): T = {
    t match {
      case TVar(a) =>
        subst.get(a) match {
          case Some(a) => apply_type(a)
          case None => t
        }
      case TFun(t1, t2) => TFun(apply_type(t1), apply_type(t2))
      case TRecord(map) => TRecord(map.map{case(k,t)=> (k,apply_type(t)) })
      case t            => t
    }
  }
  def apply_type(subst1:Subst,t:T):T = {
    val old = subst
    subst = subst1
    val t1 = apply_type(t)
    subst = old
    t1
  }
  def apply_scheme(sc:Scheme):Scheme = {
    sc match {
      case Mono(t) => Mono(apply_type(t))
      case Over => Over
    }
  }

  val nullSubst = Map[String, T]()

  var counter = 0

  def newTVar(prefix: String): T = {
    val s = counter
    counter = s + 1
    TVar(prefix + s)
  }

  def mgu(t1: T, t2: T) {
    (apply_type(t1),apply_type(t2)) match {
      case (TFun(l, r),TFun(l2, r2)) =>
        mgu(l, l2)
        mgu(r, r2)
      case (TVar(u), t) =>
        if (t != TVar(u)) {
          if (ftv_type(t).contains(u))
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
  case class TClass(name:String, members:Map[String,T],impls:Map[T,String])
  type CEnv = Map[String, TClass]

  // Main type inference function
  def ti(cenv:CEnv, env: Assumps, e: E): (CEnv, E, T) = {
    e match {
    case EVar(n) =>
      if (!env.contains(n))
        throw TypeError("unbound variable: " + n)
      env(n) match {
        case Mono(t) => (cenv, e, t)
        case Over => // オーバーロードされた型はプレースホルダにする
          val t = newTVar("'dictOver")
          (cenv, EPlaceHolder(e, t), t)
      }
      
    case EInt(_)  => (cenv, e, TInt)
    case EBool(_) => (cenv, e, TBool)
    case EAbs(n, e1) =>
      val t = newTVar("'abs")
      val env1 = env + (n -> Mono(t))
      val (cenv1, e1_, t1) = ti(cenv, env1, e1)
      (cenv1, EAbs(n, e1), TFun(t, t1))
    case EApp(e1, e2) =>
      try {
        val t = newTVar("'app")
        val (cenv1, e1_, t1) = ti(cenv, env, e1)
        val (cenv2, e2_, t2) = ti(cenv1, env, e2)
        mgu(t1, TFun(t2, t))
        (cenv2, EApp(e1_, e2_), t)
      } catch {
        case TypeError(msg) => throw TypeError(msg + "\n in " + show_e(e))
      }

    case ELet(x, e1, e2) =>
      val (cenv1, e1_, t1) = ti(cenv, env, e1)
      val env2 = env + (x -> Mono(t1))
      val (cenv2, e2_, t2) = ti(cenv1, env2, e2)
      (cenv2, ELet(x,e1_,e2_), t2)
    
    case EClass(name,p1,members,e) =>
      val cenv2 = cenv + (name -> TClass(p1, members, Map()))
      // assumpsに関数名を追加する。
      val (mems2,env2) = members.foldLeft(Map[String,T](),env) {
        case((mems,env),(name1,t)) =>
          val env2 = env + (name1 -> Over)
          val mems2 = mems + (name1->t)
          (mems2, env2)
      }
      val (cenv_, r, t) = ti(cenv2, env2, e)
      (cenv_, EType(name, List(p1), TRecord(members), r), t)

    case EInst(name, t1, members, e) =>
      cenv(name) match {
      case TClass(p, tmap, impls) =>
        val TVar(dict) = newTVar("dict_") 
        // クラス環境に追加
        val cenv2 = cenv + (name -> TClass(p, tmap, impls+(t1->dict)))
        // 余計なメソッドがないかチェック
        // インスタンスのメソッドが揃っているかチェック
        if (tmap.keys != members.keys)
          throw TypeError("instance member error "+name+" "+show_t(t1))

        // 各メンバーの型を置き換える。
        val mems2 = members.foldLeft(Map[String,E]()){case (mems,(k, e)) =>
          val old = subst
          subst = Map(p->t1)
          val instt = apply_type(tmap(k))
          subst = old
          val (cenv_, r1, instt1) = ti(cenv2, env, e)
          mgu(instt, instt1)
          (mems + (k -> r1))
        }
        val (cenv3, r2, t) = ti(cenv2, env, e)
        (cenv3, ELet(dict, ERecord(members), r2), t)
      }
    case e =>
      throw TypeError("invalid expression "+e)
    }
  }

  // Main type inference function
  def plane(cenv:CEnv, e: E): E = {
    e match {
    case EVar(n) => e
    case EInt(_)  => e
    case EBool(_) => e
    case EAbs(n, e1) => EAbs(n, plane(cenv, e1))
    case EApp(e1, e2) => EApp(plane(cenv, e1),plane(cenv, e2))
    case ELet(x, e1, e2) => ELet(x, plane(cenv, e1), plane(cenv, e2))
    case ERecord(map:Map[String,E]) => ERecord(map.map{case(k,e)=>(k,plane(cenv, e))})
    case ERecordGet(e:E,f:String) => ERecordGet(plane(cenv, e), f)
    case EType(name:String, prm:List[String], t:T, e:E) => EType(name,prm,t, plane(cenv, e))
    case EPlaceHolder(e@EVar(n), t:T) =>
      cenv.find{case(klass,tclass)=>tclass.members.contains(n)} match {
        case Some((klass,TClass(name:String, members:Map[String,T],impls:Map[T,String]))) =>
          // 一般的な型を取り出し
          val tv = newTVar("some")
          val t1 = members(n)
          val t2 = apply_type(Map(name->tv), t1)
          mgu(t, t2)
          val tva = apply_type(tv)
          ERecordGet(EVar(impls(tva)),n)
      }
    case e =>
      throw TypeError("invalid expression "+e)

    }
  }

  def type_inference(env:Assumps, e: E):(CEnv, E, T) = {
    subst = nullSubst
    val (cenv, e_, t) = ti(Map(), env, e)
    println("subst="+subst)
    println("cenv="+cenv)
    (cenv, plane(cenv, e_), apply_type(t))
  }
  // Tests
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

  def testError(e:E) {
    try {
      val (_,e_,t) = type_inference(Map(), e)
      println(show_e(e) + " :: " + show_t(t) + "\n")
      assert(false)
    } catch {
      case TypeError(err) =>
    }
  }

  // Tests
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

  def testError2(e:E) {
    try {
      val (_, e_,t) = type_inference(Map(), e)
      println(show_e(e) + " :: " + show_t(t) + "\n")
      assert(false)
    } catch {
      case TypeError(err) =>
    }
  }

  // Main Program
  test(ELet("id", EAbs("x", EVar("x")),
    EVar("id")),
    TFun(TVar("'abs0"), TVar("'abs0")))

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
    TFun(TFun(TBool,TVar("'app11")),TVar("'app11")))

  testError(EApp(EInt(2), EInt(2)))


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

