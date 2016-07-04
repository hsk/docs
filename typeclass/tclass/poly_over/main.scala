object PolyOverFun extends App {

  sealed trait E
  case class EVar(a: String) extends E
  case class EInt(a: Int) extends E
  case class EBool(a: Boolean) extends E
  case class EApp(a: E, b: E) extends E
  case class EAbs(a: String, b: E) extends E
  case class ELet(a: String, b: E, c: E) extends E
  case class EOver(a: String, ps: List[String], b: T, c: E) extends E
  case class EInst(a: String, t: T, b: E, c: E) extends E
  case class EPVar(a: String, sc: Scheme) extends E
  case class EPLet(a: String, sc: Scheme, b: E, c: E) extends E

  sealed trait T
  case class TVar(a: String) extends T
  case object TInt extends T
  case object TBool extends T
  case class TFun(a: T, b: T) extends T

  sealed trait P
  case class PName(n: String) extends P

  case class Scheme(a: List[String], t: T)

  type CEnv = Map[P, Map[T, String]]

  val nullCEnv = Map[P, Map[T, String]]()
  var cenv = nullCEnv

  val nullPreds = List[(T, P)]()
  var preds = nullPreds

  def planePreds() {
    preds = preds.map {
      case (t, p) => (apply_t(t), p)
    }
    cenv = cenv.map {
      case (k, map) =>
        val map_ = map.map {
          case (t, dict) => (apply_t(t), dict)
        }
        (k, map_)
    }
  }

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
    assumps.values.toList.map(ftv_scheme).foldRight(Set[String]()) {
      case (x, s) => s.union(x)
    }

  def apply_subst(s: Subst, t: T): T =
    t match {
      case TVar(n)      => s.getOrElse(n, TVar(n))
      case TFun(t1, t2) => TFun(apply_subst(s, t1), apply_subst(s, t2))
      case t            => t
    }

  def apply_t(t: T): T = apply_subst(subst, t)

  def apply_scheme(scheme: Scheme): Scheme = {
    scheme match {
      case Scheme(vars, t) =>
        Scheme(vars, apply_subst(vars.foldRight(subst) { case (x, s) => s - x }, t))
    }
  }

  def apply_scheme2(scheme: Scheme): (List[T], T) = {
    scheme match {
      case Scheme(vars, t) =>
        val t2 = apply_t(t)
        val tvars2 = vars.map {
          case v =>
            apply_t(TVar(v))
        }
        (tvars2, t2)
    }
  }

  def apply_assumps(assumps: Assumps): Assumps =
    assumps.map {
      case (k, v) => (k, apply_scheme(v))
    }

  def var_bind(u: String, t: T) {
    if (t != TVar(u)) {
      if (ftv_t(t).contains(u))
        throw TypeError("occurs check fails: " + u + " vs. " + show_t(t))
      subst = subst + (u -> t)
    }
  }

  def mgu(t1: T, t2: T) {
    (t1, t2) match {
      case (TFun(l, r), TFun(l2, r2)) =>
        mgu(l, l2)
        mgu(apply_t(r), apply_t(r2))
      case (TVar(u), t)   => var_bind(u, t)
      case (t, TVar(u))   => var_bind(u, t)
      case (TInt, TInt)   =>
      case (TBool, TBool) =>
      case (t1, t2) =>
        throw TypeError("types do not unify: " + show_t(t1) + " vs. " + show_t(t2))
    }
  }

  def ti(env: Assumps, e: E): (E, T) = {
    e match {
      case EInt(_)  => (e, TInt)
      case EBool(_) => (e, TBool)
      case EAbs(n, e) =>
        val tv = new_tvar("a")
        val (e1, t1) = ti(env + (n -> Scheme(List(), tv)), e)
        (EAbs(n, e1), TFun(apply_t(tv), t1))
      case EApp(e1, e2) =>
        try {
          val tv = new_tvar("a")
          val (e1_, t1) = ti(env, e1)
          val (e2_, t2) = ti(apply_assumps(env), e2)
          mgu(apply_t(t1), TFun(t2, tv))
          (EApp(e1_, e2_), apply_t(tv))
        } catch {
          case TypeError(msg) => throw TypeError(msg + "\n in " + show_e(e))
        }
      case EOver(x, ps, t, e) =>
        // 1. 型環境に関数の型スキームを追加する
        val env1 = env + (x -> Scheme(ps, t))
        // 2. 述語環境に型パラメータの述語を追加する
        ps.foreach { (p) => preds = (TVar(p), PName(x)) :: preds }
        // 3. クラス環境に名前を追加する
        cenv = cenv + (PName(x) -> Map[T, String]())
        val (e2, t2) = ti(env1, e)
        // 4. ELet(クラス名,EAbs("dict",EVar("dict")), 式2)に置き換える
        val e_ = ELet(x, EAbs("f", EVar("f")), e2)
        (e_, t2)
      case EInst(x, t, e1, e2) =>
        // 1. クラス環境からクラス名で型から辞書へのマップを取り出す
        val map = cenv.get(PName(x)) match {
          case None      => throw new Exception("type error")
          case Some(map) => map
        }
        // 2. 辞書変数名を作る
        val TVar(dict) = new_tvar("'dict")
        // 3. クラス環境に型名と辞書名を追加する
        cenv = cenv + (PName(x) -> (map + (t -> dict)))
        val (e1_, t1) = ti(env, e1)
        // 4. 型環境から型スキームを取り出し式e1の型チェックを行う
        env.get(x) match {
          case None                    => throw new Exception("type error")
          case Some(Scheme(vars, sct)) =>
        }
        val (e2_, t2) = ti(env, e2)
        // 5. ELet(辞書名,式,継続式)に置き換える
        (ELet(dict, e1_, e2_), t2)
      case EVar(n) =>
        env.get(n) match {
          case None => throw TypeError("unbound variable: " + n)
          case Some(Scheme(vars, t)) =>
            val (s, ps) = vars.foldRight(Map[String, T](), List[(T, P)]()) {
              case (v, (subst, ps)) =>
                val tv = TVar(v)
                // 1. 型スキーム中に型パラメータがあれば対応する型変数を新たに作る。
                val ntv = new_tvar("'inst_poly")
                // 2. 型変数の述語を取得する。
                val ps2 = preds.foldRight(ps) {
                  case ((t, p), ps) if (t == tv) => (ntv, p) :: ps
                  case _                         => ps
                }
                (subst + (v -> ntv), ps2)
            }
            // 型は新しい型変数に置き換える
            val t_ = apply_subst(s, t)
            if (ps == List()) {
              (e, t_)
            } else {
              // 3. 述語があれば、型と変数名を述語環境に保存する。
              preds = preds ::: ps
              // 4. 述語があれば、型が決定されていない可能性があるので
              // プレースホルダEPVar(string,sc)に置き換える。
              (EPVar(n, Scheme(s.map { case (_, TVar(n)) => n }.toList, t_)), t_)
            }
        }
      case ELet(x, e1, e2) =>
        val (e1_, t1) = ti(env, e1)
        val env1 = apply_assumps(env)

        // 1. 型から型パラメータを取り出し、型スキームを作る
        val tvars = ftv_t(t1).diff(ftv_assumps(env1)).toList
        val sc1 = Scheme(tvars, t1)

        // 2. 型パラメータから述語を取り出す。
        val ps = tvars.foldLeft(List[(T, P)]()) {
          case (ps, tv: String) =>
            preds.foldRight(ps) {
              case ((t, p), ps) =>
                if (t == TVar(tv)) (t, p) :: ps else ps
            }
        }

        if (ps != List()) {
          // 3. 述語がある場合、クラス環境に名前と述語に対する変数名を保存する。
          ps.foreach {
            case ((t, add)) =>
              val TVar(ntvar) = new_tvar(x + "_dict")
              cenv = cenv + (add -> (cenv(add) + (t -> ntvar)))
          }
        }
        val (e2_, t2) = ti(apply_assumps(env + (x -> sc1)), e2)

        if (ps == List()) {
          (ELet(x, e1_, e2_), t2)
        } else {
          // 4. 述語がある場合、式はプレースホルダEPLet(x,sc,e1,e2)に置き換える。
          (EPLet(x, sc1, e1_, e2_), t2)
        }

    }
  }
  def plane(e: E): E = {
    e match {
      case EVar(n)         => e
      case EInt(_)         => e
      case EBool(_)        => e
      case EAbs(n, e1)     => EAbs(n, plane(e1))
      case EApp(e1, e2)    => EApp(plane(e1), plane(e2))
      case ELet(x, e1, e2) => ELet(x, plane(e1), plane(e2))

      // 1. EPVar(変数名,型スキーム)の型スキームと変数名を取り出す。
      case EPVar(n, sc) =>
        // 2. 型スキームをプレーンにする。
        val (vars, t) = apply_scheme2(sc)
        // 3. 型変数でループする
        vars.foldLeft(EVar(n): E) {
          case (e, v) =>
            // 4.述語環境と型変数から、述語を求める
            preds.foldLeft(e) {
              case (e, (t, p)) if (t == v) =>
                // 5. 述語に対するクラスを求める
                val map = cenv(p)
                // 6. クラスの型パラメータに対応する、辞書を求める。
                map.get(t) match {
                  // 7. EApp(辞書,EVar(変数名))に置き換える。
                  case Some(n2) => EApp(e, EVar(n2))
                  case None     => e
                }
              case (e, _) => e
            }
        }

      case EPLet(x, sc, e1, e2) =>
        // 1. EPLet(x,sc,e1,e2)の型スキームから、型パラメータを求める。
        val (vars, t) = apply_scheme2(sc)

        // 2. 型パラメータと述語環境から述語を求める。
        val ps = vars.foldRight(List[(T, P)]()) {
          case (tv: T, ps) =>
            preds.foldRight(ps) {
              case ((t, p), ps) =>
                if (t == tv) (t, p) :: ps else ps
            }
        }

        val e1_ = ps.foldLeft(plane(e1)) {
          case (e, (t, p)) =>
            // 3. 述語からクラスの辞書を求める。
            val dict = cenv(p)(t)
            // 4. ELet(x,EAbs(EVar(辞書), e1), e2)に置き換える。
            EAbs(dict, e)
        }
        ELet(x, e1_, plane(e2))

      case e =>
        throw TypeError("invalid expression " + e)
    }
  }

  def type_inference(env: Map[String, Scheme], e: E): (CEnv, E, T) = {
    count = 0
    preds = nullPreds
    subst = nullSubst
    cenv = nullCEnv
    val (e_, t) = ti(env, e)
    planePreds()
    println("preds=" + preds)
    println("cenv=" + cenv)
    println("e=" + e_)
    (cenv, plane(e_), apply_t(t))
  }

  def test(e: E, et: T) {
    try {
      val (_, _, t) = type_inference(Map(), e)
      println("t=" + t)
      assert(t == et)
    } catch {
      case TypeError(err) =>
        println(show_e(e) + "\n " + err + "\n")
        assert(false)
    }
  }

  def test_error(e: E) {
    try {
      type_inference(Map(), e)
      assert(false)
    } catch {
      case TypeError(err) =>
    }
  }

  def test2(env: Assumps, e: E) {
    try {
      val (cenv, e_, t) = type_inference(env, e)
      println("t=" + t)
      println("e=" + e_)
    } catch {
      case TypeError(err) =>
        println(show_e(e) + "\n " + err + "\n")
        assert(false)
    }
  }

  test(ELet("id", EAbs("x", EVar("x")),
    EVar("id")),
    TFun(TVar("'inst_poly1"), TVar("'inst_poly1")))

  test(ELet("id", EAbs("x", EVar("x")),
    EApp(EVar("id"), EVar("id"))),
    TFun(TVar("'inst_poly3"), TVar("'inst_poly3")))

  test(ELet("id", EAbs("x", ELet("y", EVar("x"), EVar("y"))),
    EApp(EVar("id"), EVar("id"))),
    TFun(TVar("'inst_poly3"), TVar("'inst_poly3")))

  test(ELet("id", EAbs("x", ELet("y", EVar("x"), EVar("y"))),
    EApp(EApp(EVar("id"), EVar("id")), EInt(2))),
    TInt)

  test_error(ELet("id", EAbs("x", EApp(EVar("x"), EVar("x"))),
    EVar("id")))

  test(EAbs("m", ELet("y", EVar("m"),
    ELet("x", EApp(EVar("y"), EBool(true)),
      EVar("x")))),
    TFun(TFun(TBool, TVar("a1")), TVar("a1")))

  test_error(EApp(EInt(2), EInt(2)))

  test2(
    Map("+" -> Scheme(List(), TFun(TInt, TFun(TInt, TInt)))),

    EOver("add", List("a"), TFun(TVar("a"), TFun(TVar("a"), TVar("a"))),
      EInst("add", TInt, EAbs("x", EAbs("y", EApp(EApp(EVar("+"), EVar("x")), EVar("y")))),
        EApp(EApp(EVar("add"), EInt(1)), EInt(2)))))

  test2(
    Map("+" -> Scheme(List(), TFun(TInt, TFun(TInt, TInt)))),

    EOver("add", List("'a"), TFun(TVar("'a"), TFun(TVar("'a"), TVar("'a"))),
      EInst("add", TInt, EAbs("x", EAbs("y", EApp(EApp(EVar("+"), EVar("x")), EVar("y")))),
        ELet("a", EVar("add"), EApp(EApp(EVar("a"), EInt(1)), EInt(2))))))
}
