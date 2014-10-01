(* 3 Kinds *)
type kind =
| Star
| Kfun of kind * kind

let rec show (k:kind):string =
begin match k with
  | Star -> "*"
  | Kfun(Kfun _ as k1,k2) -> Printf.sprintf "(%s) -> %s" (show k1) (show k2) 
  | Kfun(k1,k2) -> Printf.sprintf "%s -> %s" (show k1) (show k2) 
end

let rec show_list (ks:kind list):string =
Pre.show_list show ";" ks

(*|

# Kind

    >>> open Kind;;
    
    >>> Star ;;
    - : Kind.kind = Star

    >>> Kfun(Star, Star) ;;
    - : Kind.kind = Kfun (Star, Star)

    >>> Kfun(Star, Kfun(Star, Star)) ;;
    - : Kind.kind = Kfun (Star, Kfun (Star, Star))

    >>> Kfun(Star, Kfun(Star, Kfun(Star, Star))) ;;
    - : Kind.kind = Kfun (Star, Kfun (Star, Kfun (Star, Star)))

    >>> Kfun(Kfun(Star, Star), Kfun(Star, Star)) ;;
    - : Kind.kind = Kfun (Kfun (Star, Star), Kfun (Star, Star))

    >>> Kfun(Star,Kfun(Kfun(Star, Star), Star)) ;;
    - : Kind.kind = Kfun (Star, Kfun (Kfun (Star, Star), Star))

*)
