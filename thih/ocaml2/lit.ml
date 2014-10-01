(* 11.1 Literals *)
(*|

    >>> open Lit;;
    
    >>> 1;;
    - : int = 1
*)

  open Kind
  open Type
  open Pred
  open TIMonad
  open Infer
  open Big_int
  open Num
  type literal =
    | LitInt of int
    | LitChar of char
    | LitRat of num
    | LitStr of string

  let tiLit (ti:ti) (lit:literal):pred list * type_ =
    begin match lit with
      | LitChar _ -> ([], tChar)
      | LitInt _ ->
        let v = newTVar ti Star in
        ([IsIn("Num", v)], v)
      | LitStr _ -> ([], tString)
      | LitRat _ ->
        let v = newTVar ti Star in
        ([IsIn("Fractional", v)], v)
    end

(*|

    >>> Lit.LitInt 123;;
    - : Lit.literal = LitInt 123

    >>> LitInt 123;;
    - : Lit.literal = LitInt 123

    >>> runTI begin fun ti ->
        let lit = LitInt 123 in
        let (preds, ty) = tiLit(ti)(lit) in
        let subst = getSubst(ti) in
        let ty2 = Subst_.typeApply(subst)(ty) in

        (preds = [IsIn("Num", TVar(Tyvar("v0", Star)))],
        ty = TVar(Tyvar("v0", Star)),
        ty2 = ty,
        subst = []) = (true, true, true, true)

      end;;
    - : bool = true


*)
