module Id = struct
  type id = string

  (* 数値に対するidを取得する *)
  let enumId (n:int) : id =
    "v" ^ string_of_int n
end

(* 3 Kinds *)
module Kind = struct
  type kind =
    | Star
    | Kfun of kind * kind

  let rec show (k:kind):string =
    begin match k with
      | Star -> "*"
      | Kfun(Kfun _ as k1,k2) -> Printf.sprintf "(%s) -> %s" (show k1) (show k2) 
      | Kfun(k1,k2) -> Printf.sprintf "%s -> %s" (show k1) (show k2) 
    end

end

(* 4 Types *)
module Type = struct
  open Kind
  (* 型変数 *)
  type tyvar = Tyvar of Id.id * kind
  (* 型コンストラクタ *)
  type tycon = Tycon of Id.id * kind
  (* 型 *)
  type type_ =
    | TVar of tyvar
    | TCon of tycon
    | TAp of type_ * type_
    | TGen of int

  let tUnit :type_ = TCon(Tycon("()", Star))
  let tChar :type_ = TCon(Tycon("Char", Star))
  let tInt :type_ = TCon(Tycon("Int", Star))
  let tInteger :type_ = TCon(Tycon("Integer", Star))
  let tFloat :type_ = TCon(Tycon("Float", Star))
  let tDouble :type_ = TCon(Tycon("Double", Star))

  let tList :type_ = TCon(Tycon("[]", Kfun(Star, Star)))
  let tArrow :type_ = TCon(Tycon("(->)", Kfun(Star, Kfun(Star, Star))))
  let tTuple2 :type_ = TCon(Tycon("(,)", Kfun(Star, Kfun(Star, Star))))

  let fn (a:type_) (b:type_) :type_ = TAp(TAp(tArrow, a), b)

  let list t :type_ = TAp(tList, t)

  let tString :type_ = list tChar

  let pair a b :type_ = TAp(TAp(tTuple2, a), b)

  let tyvarKind (Tyvar(_, k)) :kind = k
  let tyconKind (Tycon(_, k)) :kind = k
  let rec typeKind t:kind =
    match t with
    | TCon tc -> tyconKind tc
    | TVar u -> tyvarKind u
    | TAp(t, _) ->
      begin match typeKind t with
        | Kfun(_, k) -> k
        | _ -> failwith "inconsistent type"
      end
    | TGen _ -> failwith "generic type variables have no kind"

  let rec show (t:type_): string =
    begin match t with
      | TVar(Tyvar(id,kind)) -> Printf.sprintf "TVar(Tyvar(%s,%s))" id (Kind.show kind)
      | TCon(Tycon(id,kind)) -> Printf.sprintf "TCon(Tycon(%s,%s))" id (Kind.show kind)
      | TAp(t1,t2)           -> Printf.sprintf "TAp(%s,%s)" (show t1) (show t2)
      | TGen(i)              -> Printf.sprintf "TGen(%d)" i
    end
end


let _ =
  Printf.printf "%-14s%-12s%s\n" "name" "kind" "type_";
  let show s t =
    Printf.printf "%-14s%-12s%s\n" s (Kind.show (Type.typeKind t)) (Type.show t) 
  in
  show "tUnit" Type.tUnit;
  show "tChar" Type.tChar;
  show "tInt" Type.tInt;
  show "tInteger" Type.tInteger;
  show "tFloat" Type.tFloat;
  show "tDouble" Type.tDouble;
  show "tList" Type.tList;
  show "tArrow" Type.tArrow;
  show "tTuple2" Type.tTuple2;
  let fn_int_int = Type.fn Type.tInt Type.tInt in

  show "fn_int_int" fn_int_int;

  let list_int = Type.list Type.tInt in
  show "list_int" list_int;

  show "tString" Type.tString;

  let pair_int_char = Type.pair Type.tInt Type.tChar in
  show "pair_int_char" pair_int_char;
