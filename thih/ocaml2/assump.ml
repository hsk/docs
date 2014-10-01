(* 9 Assumptions *)
(*|
## assump

    >>> open Assump;;
    
*)

open Scheme
open Type
open Kind

type assump = Assump of Id.id * scheme

let (=::) id t = Assump(id, toScheme t)

let assumpApply (s:Subst_.subst) (Assump(i, sc):assump) : assump =
  Assump(i, schemeApply s sc)

let assumpTv (Assump(_, sc):assump):Type.tyvar list =
  schemeTv sc

let assumpsApply (s:Subst_.subst) (ass:assump list): assump list =
  Subst_.listApply assumpApply s ass

let assumpsTv (ass:assump list): Type.tyvar list =
  Subst_.listTv assumpTv ass

let find (i:Id.id) (ass:assump list): scheme =
  let Assump(_, sc) = List.find begin fun (Assump(i', _)) ->
    i = i'
  end ass in
  sc

(*|

    >>> let t = TVar(Tyvar("a", Star)) ;;
    val t : Type.type_ = TVar (Tyvar ("a", Star))

    >>> let assump = Assump("ABC", Forall([], Qual([], t))) ;;
    val assump : Assump.assump = Assump ("ABC", Forall ([], Qual ([], TVar (Tyvar ("a", Star)))))

    >>> assump = Assump("ABC", Forall([], Qual([], TVar(Tyvar("a", Star)))));;
    - : bool = true

    >>> "ABC" =:: tInt ;;
    - : Assump.assump = Assump ("ABC", Forall ([], Qual ([], TCon (Tycon ("Int", Star)))))

## assumpApply

    >>> let t = TVar(Tyvar("a", Star)) ;;
    val t : Type.type_ = TVar (Tyvar ("a", Star))

    >>> let assump = Assump("ABC", Forall([], Qual([], t))) ;;
    val assump : Assump.assump = Assump ("ABC", Forall ([], Qual ([], TVar (Tyvar ("a", Star)))))

    >>> let subst: subst = [(Tyvar("a", Star), tInt)] ;;
    val subst : Subst_.subst = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

    >>> let assump2 = assumpApply(subst)(assump) ;;
    val assump2 : Assump.assump = Assump ("ABC", Forall ([], Qual ([], TCon (Tycon ("Int", Star)))))

    >>> assump2 = Assump("ABC", Forall([], Qual([], TCon(Tycon("Int", Star)))));;
    - : bool = true

## assumpTv

    >>> let t = TVar(Tyvar("a", Star)) ;;
    val t : Type.type_ = TVar (Tyvar ("a", Star))

    >>> let assump = Assump("ABC", Forall([], Qual([], t))) ;;
    val assump : Assump.assump = Assump ("ABC", Forall ([], Qual ([], TVar (Tyvar ("a", Star)))))

    >>> let tvs = assumpTv(assump) ;;
    val tvs : Type.tyvar list = [Tyvar ("a", Star)]

    >>> tvs = [Tyvar("a", Star)] ;;
    - : bool = true

## assumpsApply

    >>> let t = TVar(Tyvar("a", Star)) ;;
    val t : Type.type_ = TVar (Tyvar ("a", Star))

    >>> let assump = Assump("ABC", Forall([], Qual([], t))) ;;
    val assump : Assump.assump = Assump ("ABC", Forall ([], Qual ([], TVar (Tyvar ("a", Star)))))

    >>> let subst = [(Tyvar("a", Star), tInt)] ;;
    val subst : (Type.tyvar * Type.type_) list = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

    >>> let assumps = assumpsApply(subst)([assump]) ;;
    val assumps : Assump.assump list = [Assump ("ABC", Forall ([], Qual ([], TCon (Tycon ("Int", Star)))))]

    >>> assumps =
        [
          Assump("ABC",
            Forall([], Qual([], TCon(Tycon("Int", Star)))))];;
    - : bool = true

## assumpsTv

    >>> let t = TVar(Tyvar("a", Star)) ;;
    val t : Type.type_ = TVar (Tyvar ("a", Star))

    >>> let assump = Assump("ABC", Forall([], Qual([], t))) ;;
    val assump : Assump.assump = Assump ("ABC", Forall ([], Qual ([], TVar (Tyvar ("a", Star)))))

    >>> let tvs = assumpsTv([assump]) ;;
    val tvs : Type.tyvar list = [Tyvar ("a", Star)]

    >>> tvs = [Tyvar("a", Star)] ;;
    - : bool = true

## find

    >>> let t = TVar(Tyvar("a", Star)) ;;
    val t : Type.type_ = TVar (Tyvar ("a", Star))

    >>> let assump = Assump("ABC", Forall([], Qual([], t))) ;;
    val assump : Assump.assump = Assump ("ABC", Forall ([], Qual ([], TVar (Tyvar ("a", Star)))))

    >>> let assump2 = Assump("ABC2", Forall([], Qual([], tInt))) ;;
    val assump2 : Assump.assump = Assump ("ABC2", Forall ([], Qual ([], TCon (Tycon ("Int", Star)))))

    >>> let sc = find("ABC")([assump;assump2]) ;;
    val sc : Scheme.scheme = Forall ([], Qual ([], TVar (Tyvar ("a", Star))))

    >>> sc = Forall([], Qual([], TVar(Tyvar("a", Star))));;
    - : bool = true

*)