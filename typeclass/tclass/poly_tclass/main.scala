object Infer extends App {

  sealed trait E
  case class EVar(a: String) extends E
  case class EInt(a: Int) extends E
  case class EBool(a: Boolean) extends E
  case class EApp(a: E, b: E) extends E
  case class EAbs(a: String, b: E) extends E
  case class ELet(a: String, b: E, c: E) extends E

  case class ERecord(map: Map[String, E]) extends E
  case class ERecordGet(e: E, f: String) extends E
  case class EType(name: String, prm: List[String], t: T, e: E) extends E

  case class EPlaceHolder(x: String, t: T, e: E) extends E
  case class EGenPlaceHolder(a:Assumps, e: E, t: T) extends E
  case class EClass(name: String, p: String, members: Map[String, T], e: E) extends E
  case class EInst(name: String, p: T, members: Map[String, E], e: E) extends E
  sealed trait T
  case class TVar(a: String) extends T
  case object TInt extends T
  case object TBool extends T
  case class TFun(a: T, b: T) extends T
  case class TRecord(t: Map[String, T]) extends T
  case class TCon(a: String, ts: List[T]) extends T
  case class TPlaceHolder(t: T) extends T

  sealed trait P
  case class PClass(_1: String) extends P
  case class PType(_1: T) extends P

  sealed trait Scheme
  case class Poly(a: List[String], ps: Map[T, Set[P]], t: T) extends Scheme
  case class Over(t: T) extends Scheme

  type Subst = Map[String, T]
  type Assumps = Map[String, Scheme]

  case class TClass(name: String, members: Map[String, T], impls: Map[T, String])
  type CEnv = Map[String, TClass]

  case class TypeError(s: String) extends Exception(s)

  def show_t(t: T): String = t.toString
  def show_e(e: E): String = e.toString

  val nullSubst = Map[String, T]()
  var subst = nullSubst

  val nullPreds = Map[T, Set[P]]()
  var preds = nullPreds

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
      case TRecord(map) =>
        map.foldLeft(Set[String]()) { case (tvs, (k, t)) => tvs.union(ftv_t(t)) }
    }

  def ftv_scheme(scheme: Scheme): Set[String] =
    scheme match {
      case Poly(vars, ps, t) => ftv_t(t).diff(vars.toSet)
      case Over(t)           => ftv_t(t)
    }

  def ftv_assumps(assumps: Assumps): Set[String] =
    assumps.values.toList.map(ftv_scheme).foldRight(Set[String]()) {
      case (x, s) => s.union(x)
    }

  def apply_subst(s: Subst, t: T): T =
    t match {
      case TVar(n)      => s.get(n) match { case None => TVar(n) case Some(t) => apply_subst(s, t) }
      case TFun(t1, t2) => TFun(apply_subst(s, t1), apply_subst(s, t2))
      case TRecord(map) => TRecord(map.map { case (k, t) => (k, apply_subst(s, t)) })
      case t            => t
    }

  def apply_t(t: T): T = apply_subst(subst, t)

  def apply_scheme(scheme: Scheme): Scheme = {
    scheme match {
      case Poly(vars, ps, t) =>
        Poly(vars, ps, apply_subst(vars.foldRight(subst) { case (x, s) => s - x }, t))
      case Over(t) => Over(apply_t(t))
    }
  }

  def apply_assumps(assumps: Assumps): Assumps =
    assumps.map {
      case (k, v) => (k, apply_scheme(v))
    }

  // 全ての型変数をフラットな状態にする
  def apply_ts(ts: List[T]): List[T] = ts.map(apply_t)

  // tのpredsを取得
  def apply_preds(t: T, preds: Map[T, Set[P]]): Set[P] = {
    preds.get(t) match {
      case None => Set()
      case Some(ps) =>
        ps.foldLeft(Set[P]()) {
          case (ps, PType(t)) => ps ++ apply_preds(t, preds)
          case (ps, p)        => ps + p
        }
    }
  }

  def apply_ps(preds: Map[T, Set[P]]): Map[T, Set[P]] = {
    // 型クラス制約の型をフラットにする
    val preds1 = preds.foldLeft(Map[T, Set[P]]()) {
      case (map, (t, ps: Set[P])) =>
        val t_ = apply_t(t)
        val ps_ = ps.map {
          case PType(t2) => PType(apply_t(t2))
          case p         => p
        }
        map.get(t_) match {
          case None             => map + (t_ -> ps_)
          case Some(ps: Set[P]) => map + (t_ -> (ps ++ ps_))
        }
    }
    preds1
    /*
    // 型クラス制約の型のリンクを消す
    preds1.map{
      case (t, ps) =>
        (t, ps.foldLeft(Set[P]()){
          case (ps,PType(t2)) => ps++apply_preds(t2, preds1)
          case (ps,p) => ps+p
        })
    }*/
  }

  /*def solve_tclasses(preds:Map[T,Set[P]], ts:List[T]):(T, Set[P]) = {
    // 型をフラットにする
    val preds1 = apply_ps(preds)
    ts.map{ t =>
        val t1 = apply_t(t)
        (t1, preds1(t1))
    }
  }
  assert (      
    List(TCon("int",List()),List()) == solveTClasses(
      [TVar "t1", TVar "t2"; TVar "t2", TInt; TVar "t3", TVar "t1"]
      [TVar "t1", PClass "Num"; TVar "t2", PType "t3"; TVar "t3", PClass "Show"]
      [TVar "t1"])
  )*/

  def add_p(t: T, p: P) {
    preds.get(t) match {
      case None     => preds += (t -> Set(p))
      case Some(ps) => preds += (t -> (ps + p))
    }
  }

  def generalize(cenv: CEnv, env: Assumps, e: E, t: T): (CEnv, E, Scheme) = {
    t match {
      case TPlaceHolder(t) =>
        (cenv, EGenPlaceHolder(env, e, t), Over(TPlaceHolder(t)))
      case _ =>
        val vars = ftv_t(t).diff(ftv_assumps(env)).toList
        // predsをまとめる
        preds = apply_ps(preds)
        val ps = vars.foldLeft(Map[T, Set[P]]()) { (map, s) =>
          preds.get(TVar(s)) match {
            case None => map
            case Some(ps) =>
              ps.foreach { p => add_p(TVar(s), p) }
              map.get(TVar(s)) match {
                case None      => map + (TVar(s) -> ps)
                case Some(ps_) => map + (TVar(s) -> (ps_ ++ ps))
              }
          }
        }
        (cenv, e, Poly(vars, ps, t))
    }
  }

  def instantiate(cenv: CEnv, env: Assumps, e: E, n: String): (CEnv, E, T) = {
    env(n) match {
      case Poly(vars, ps, t) =>
        val s = vars.foldLeft(Map[String, T]()) {
          case (subst, k) =>
            val t = new_tvar("a")
            // 述語があれば、コピーする
            ps.get(t) match {
              case None     =>
              case Some(ps) => ps.foreach { p => add_p(t, p) }
            }
            subst + (k -> t)
        }
        (cenv, e, apply_subst(s, t))
      case Over(t) => // オーバーロードされた型はプレースホルダにする
        val t1 = TPlaceHolder(new_tvar("'dictOver_instantiate"))
        println("over e=" + e + " t=" + t + " " + t1)
        add_p(t1, PType(t)) // 述語を追加
        (cenv, EPlaceHolder(n, t1, e), t1)
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
    (apply_t(t1), apply_t(t2)) match {
      case (TFun(l, r), TFun(l2, r2)) =>
        mgu(l, l2)
        mgu(r, r2)
      case (TVar(u), t)   => var_bind(u, t)
      case (t, TVar(u))   => var_bind(u, t)
      case (TInt, TInt)   =>
      case (TBool, TBool) =>
      case (t @ TRecord(map1), u @ TRecord(map2)) =>
        if (map1.keys != map2.keys)
          throw new TypeError("cannot unify " + show_t(t) + " with " + show_t(u))
        map1.foreach {
          case (k, t) =>
            mgu(map2(k), t)
        }
      case (TPlaceHolder(t), u) => mgu(t, u)
      case (t, TPlaceHolder(u)) => mgu(t, u)
      case (t1, t2) =>
        throw TypeError("types do not unify: " + show_t(t1) + " vs. " + show_t(t2))
    }
  }

  def ti(cenv: CEnv, env: Assumps, e: E): (CEnv, E, T) = {
    e match {
      case EVar(n) =>
        if (!env.contains(n))
          throw TypeError("unbound variable: " + n)
        instantiate(cenv, env, e, n)
      case EInt(_)  => (cenv, e, TInt)
      case EBool(_) => (cenv, e, TBool)
      case EAbs(n, e1) =>
        val t = new_tvar("'abs")
        val (cenv1, e1_, t1) = ti(cenv, env + (n -> Poly(List(), Map(), t)), e1)
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
        println("ELet t1=" + t1)
        // 一般化する
        val (cenv1_, e1__, sc1) = generalize(cenv1, apply_assumps(env), e1_, t1)
        // xにはsc1をバインドするけど、型はなくて型スキームがある
        // addの一般的な型が
        val (cenv2, e2_, t2) = ti(cenv1_, apply_assumps(env + (x -> sc1)), e2)
        (cenv2, ELet(x, e1__, e2_), t2)
      case EClass(name, p1, members, e) =>
        val cenv2 = cenv + (name -> TClass(p1, members, Map()))
        val env2 = members.foldLeft(env) {
          case (env, (name1, _)) =>
            env + (name1 -> Over(new_tvar("'eclass_" + name1)))
        }
        val (cenv_, r, t) = ti(cenv2, env2, e)
        val r2 = members.foldLeft(r) {
          case (r, (name: String, _)) =>
            val TVar(dict: String) = new_tvar("'dict")
            ELet(name, EAbs(dict, ERecordGet(EVar(dict), name)), r)
        }
        (cenv_, EType(name, List(p1), TRecord(members), r2), t)

      case EInst(name, t1, members, e) =>
        cenv(name) match {
          case TClass(p, tmap, impls) =>
            val TVar(dict) = new_tvar("dict_")
            val cenv2 = cenv + (name -> TClass(p, tmap, impls + (t1 -> dict)))
            if (tmap.keys != members.keys)
              throw TypeError("instance member error " + name + " " + show_t(t1))
            members.foreach {
              case (k, e) =>
                val (cenv_, r1, instt1) = ti(cenv2, env, e)
                val instt = apply_subst(Map(p -> t1), tmap(k))
                mgu(instt, instt1)
            }
            val (cenv3, r2, t) = ti(cenv2, env, e)
            (cenv3, ELet(dict, ERecord(members), r2), t)
        }
      case e =>
        throw TypeError("invalid expression " + e)

    }
  }

  def plane(cenv: CEnv, e: E): E = {
    e match {
      case EVar(n)                => e
      case EInt(_)                => e
      case EBool(_)               => e
      case EAbs(n, e1)            => EAbs(n, plane(cenv, e1))
      case EApp(e1, e2)           => EApp(plane(cenv, e1), plane(cenv, e2))
      case ELet(x, e1, e2)        => ELet(x, plane(cenv, e1), plane(cenv, e2))
      case ERecord(map)           => ERecord(map.map { case (k, e) => (k, plane(cenv, e)) })
      case ERecordGet(e, f)       => ERecordGet(plane(cenv, e), f)
      case EType(name, prm, t, e) => EType(name, prm, t, plane(cenv, e))

      case EGenPlaceHolder(env, e, t) =>
        val (cenv2, e2, sc)=generalize(cenv, env, e, t)
        println("sc="+sc+" e="+e+" env(t)="+env)
        // ここで置き換えが必要。型が分からないので駄目。
        plane(cenv, EApp(EVar("sss"),e2))
      case EPlaceHolder(n, t, e) =>
        cenv.find { case (klass, tclass) => tclass.members.contains(n) } match {
          case Some((klass, TClass(name: String, members: Map[String, T], impls: Map[T, String]))) =>
            println("n=" + n)
            println("t=" + t)
            println("subst=" + subst)
            println("apply_t(t)=" + apply_t(t))
            println("preds(t)=" + preds(t))
            println("e=" + e)
            val tv @ TVar(some) = new_tvar("some")
            val t1 = apply_subst(Map(name -> tv), members(n))
            mgu(t1, apply_t(t))
            println(t1)
            val tv_ = apply_t(tv)
            println("tv_=" + tv_)
            println("impls=" + impls)
            impls.get(tv_) match {
              case Some(s) =>
                EApp(EVar(n), EVar(impls(tv_)))
              case None =>
                EApp(EVar(n), EVar(some))

            }
          case None => EVar(n)
        }
      case e =>
        throw TypeError("invalid expression " + e)
    }
  }

  def type_inference(env: Map[String, Scheme], e: E): (CEnv, E, T) = {
    println("=================")
    subst = nullSubst
    preds = nullPreds
    count = 0
    val (cenv, e_, t) = ti(Map(), env, e)
    (cenv, plane(cenv, e_), apply_t(t))
  }

  def test(e: E, et: T) {
    try {
      val (_, e_, t) = type_inference(Map(), e)
      if (t != et) println(show_e(e) + " :: " + show_t(t) + "\n")
      assert(t == et)
    } catch {
      case TypeError(err) =>
        println(show_e(e) + "\n " + err + "\n")
        assert(false)
    }
  }

  def test_error(e: E) {
    try {
      val (_, e_, t) = type_inference(Map(), e)
      println(show_e(e) + " :: " + show_t(t) + "\n")
      assert(false)
    } catch {
      case TypeError(err) =>
    }
  }

  def test2(env: Map[String, Scheme], e: E, ee: E): Boolean = {
    try {
      val (cenv_, e_, t) = type_inference(env, e)
      println(cenv_)
      println(e_)
      e_ == ee
    } catch {
      case TypeError(err) =>
        println(show_e(e) + "\n " + err + "\n")
        false
    }
  }

  def test3(env: Map[String, Scheme], e: E): Boolean = {
    try {
      val (_, e_, t) = type_inference(env, e)
      println(show_e(e_) + " :: " + show_t(t) + "\n")
      true
    } catch {
      case TypeError(err) =>
        println("error " + err)
        false
    }
  }
  /*
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
    TFun(TFun(TBool,TVar("'app18")),TVar("'app18")))

  test_error(EApp(EInt(2), EInt(2)))*/

  assert(test2(
    Map("+" -> Poly(List(), Map(), TFun(TInt, TFun(TInt, TInt)))),

    EClass("Num", "a", Map("add" -> TFun(TVar("a"), TFun(TVar("a"), TVar("a")))),
      EInst("Num", TInt, Map("add" -> EAbs("x", EAbs("y", EApp(EApp(EVar("+"), EVar("x")), EVar("y"))))),
        EApp(EApp(EVar("add"), EInt(1)), EInt(2)))),

    EType("Num", List("a"), TRecord(Map(
      "add" -> TFun(TVar("a"), TFun(TVar("a"), TVar("a"))))),
      ELet("add", EAbs("'dict9", ERecordGet(EVar("'dict9"), "add")),
        ELet("dict_1", ERecord(Map(
          "add" -> EAbs("x", EAbs("y", EApp(EApp(EVar("+"), EVar("x")), EVar("y")))))),
          EApp(EApp(EApp(EVar("add"), EVar("dict_1")), EInt(1)), EInt(2)))))))

  /*
  assert(test3(
    Map("+"->Poly(List(), Map(), TFun(TInt,TFun(TInt,TInt)))),

    EClass("Num","a",Map("add"->TFun(TVar("a"), TFun(TVar("a"), TVar("a")))),
    EInst("Num",TInt,Map("add"->EAbs("x",EAbs("y",EApp(EApp(EVar("+"),EVar("x")),EVar("y"))))),
    ELet("a",EVar("add"),
    EApp(EApp(EVar("a"),EInt(1)),EInt(2)))))))
*/

  assert(test3(
    Map("+" -> Poly(List(), Map(), TFun(TInt, TFun(TInt, TInt)))),

    EClass("Num", "a", Map("add" -> TFun(TVar("a"), TFun(TVar("a"), TVar("a")))),
      EInst("Num", TInt, Map("add" -> EAbs("x", EAbs("y", EApp(EApp(EVar("+"), EVar("x")), EVar("y"))))),
        ELet("add1", EVar("add"), EVar("add1"))))))
}
