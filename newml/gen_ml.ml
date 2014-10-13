open Format
open Ast

let rec print_ls sep p ppf = function
  | [] -> ()
  | [x] -> p ppf x
  | x::xs ->
    fprintf ppf "%a%s%a" p x sep (print_ls sep p) xs

let rec print_t pp sp ppf = function
  | TEmpty -> ()
  | Ty(s) ->
    fprintf ppf "%s%s" <|_2(
      sp,
      s
    )
  | TTuple(ls) ->
    fprintf ppf "%s(%a)" 
      sp
      (print_ls " * " (print_t "" "")) ls
    
  | TFun(t1,t2) ->
    fprintf ppf "(%a)->(%a)" 
      (print_t "" sp) t1
      (print_t "" sp) t2
  | TUnit ->
    fprintf ppf "unit"
  | TGen(t,ts) ->
    fprintf ppf "%s(%a) %a"
      sp
      (print_ls " * " (print_t "" "")) ts
      (print_t "" "") t

let rec print_e sp ppf = function

  | EEmpty ->
    ()
  | EUnit ->
    fprintf ppf "%s()" sp
  | EInt i ->
    fprintf ppf "%s%d" <|_2(
      sp,
      i
    )
    
  | EString i ->
    fprintf ppf "%s%s" <|_2(
      sp,
      i
    )

  | EVar i ->
    fprintf ppf "%s%s" <|_2(
      sp,
      i
    )

  | EBin(e1,op,e2) ->
    fprintf ppf "%s(%a %s %a)" <|_6(
      sp,
      print_e "", e1,
      op,
      print_e "", e2
    )
  | EPre(op,e1) ->
    fprintf ppf "%s(%s %a)" <|_4(
      sp,
      op,
      print_e "", e1
    )
  | ECall(e1,es) ->
    let rec print_ls sep p ppf = function
      | [] -> ()
      | [x] -> fprintf ppf "(%a)" p x
      | x::xs ->
        fprintf ppf "(%a)%s%a"
          p x
          sep
          (print_ls sep p) xs
    in
    fprintf ppf "%a%a" <|_4(
      print_e sp, e1,
      print_ls<|_2(" ", print_e ""), es
    )

  | EIf(e1, e2, EEmpty) ->
    fprintf ppf "%s(if %a then (%a)%s;)"
      sp
      (print_e "") e1
      (print_e_block sp "\n") e2
      sp

  | EIf(e1, e2, e3) ->
    fprintf ppf "%s(if %a then (%a%s)else(%a))"
      sp
      (print_e "" ) e1
      (print_e_block sp "\n") e2 
      sp
      (print_e_block sp "") e3 

  | EFun (its, t, e) ->
    let rec print_ls sep p ppf = function
      | [] -> ()
      | [x] -> fprintf ppf "(%a)" p x
      | x::xs ->
        fprintf ppf "(%a)%s%a"
          p x
          sep
          (print_ls sep p) xs
    in
    fprintf ppf "(fun %a%a -> %a)"
      (print_ls " " (print_e " ")) its
      (print_t (if t=TEmpty then "" else ":") "") t
      (print_e_block sp "\n") e

  | EPFun (ls) ->
    fprintf ppf "(function ";
    ls|>List.iter begin function
      | EFun(its, t, e) ->
        let rec print_ls sep p ppf = function
          | [] -> ()
          | [x] -> fprintf ppf "(%a)" p x
          | x::xs ->
            fprintf ppf "(%a)%s%a"
              p x
              sep
              (print_ls sep p) xs
        in
        fprintf ppf "| %a%a -> (%a)"
          (print_ls " " (print_e " ")) its
          (print_t (if t=TEmpty then "" else ":") "") t
          (print_e_block sp "\n") e


      | _ -> assert false
    end;
    fprintf ppf ")"
  | EMatch (e,ls) ->
    fprintf ppf "(match %a with "
      (print_e "") e
      ;
    ls|>List.iter begin function
      | EFun(its, t, e) ->
        let rec print_ls sep p ppf = function
          | [] -> ()
          | [x] -> fprintf ppf "(%a)" p x
          | x::xs ->
            fprintf ppf "(%a)%s%a"
              p x
              sep
              (print_ls sep p) xs
        in
        fprintf ppf "| %a%a -> (%a)"
          (print_ls " " (print_e " ")) its
          (print_t (if t=TEmpty then "" else ":") "") t
          (print_e_block sp "\n") e


      | _ -> assert false
    end;
    fprintf ppf ")"
  | ETypeRec(id, ls) ->
    fprintf ppf "type %s = {%a}"
      id
      (print_ls ";"
        (fun ppf (s,t) -> fprintf ppf "%s:%a" s (print_t "" "") t)
      )
      ls
  | ETypeVariant(id, ls) ->
    fprintf ppf "type %s = %a"
      id
      (print_ls "|"
        (fun ppf -> function
          |(s,TEmpty) -> fprintf ppf "%s" s
          |(s,t) -> fprintf ppf "%s of %a" s (print_t "" "") t
        )
      )
      ls
  | ERecord(ls) ->
    fprintf ppf "{%a}"
      (print_ls ";"
        (fun ppf -> function
          | (s,EEmpty) -> fprintf ppf "%s" s
          | (s,e) -> fprintf ppf "%s=%a" s (print_e "") e
        )
      )
      ls
  | EBlock ls ->
    let rec loop e = match e with
      | [] -> ()
      | [ELetRec _ as e] | [ELet _ as e] ->
        fprintf ppf "%a in ()@." (print_e sp) e
      | (ELetRec _ as e)::xs | (ELet _ as e)::xs ->
        fprintf ppf "%a in@." (print_e sp) e;
        loop xs
      | e::xs ->
        fprintf ppf "%a;@." (print_e sp) e;
        loop xs
    in
    loop ls

  | EList ls ->
    fprintf ppf "[%a]"
      (print_ls "; " (print_e "")) ls

  | ELet (id, TEmpty, e) ->
    fprintf ppf "%slet %s = %a"
      sp
      id
      (print_e "") e
  | ELet (id, t, e) ->
    fprintf ppf "%slet (%s:%a) = %a"
      sp
      id
      (print_t "" "") t
      (print_e "") e
  | ELetRec (id, TEmpty, e) ->
    fprintf ppf "%slet rec %s = %a"
      sp
      id
      (print_e "") e
  | ELetRec (id, t, e) ->
    fprintf ppf "%slet rec (%s:%a) = %a"
      sp
      id
      (print_t "" "") t
      (print_e "") e

(*

  | ECallM(i,e1,es) ->
    fprintf ppf "%a(%a)"<|_4(
      print_e sp, e1,
      print_ls<|_2(", ", print_e ""), es
    )
  | EArr(e1,es) ->
    fprintf ppf "%a[%a]"<|_4(
      print_e sp, e1,
      print_ls<|_2(", ", print_e ""), es
    )
  | ECast(t,e) ->
    fprintf ppf "((%a)%a)"<|_4(
      print_t "" sp, t,
      print_e "", e
    )
*)
and print_e_block sp b ppf = print_e sp ppf

let rec print_s ppf (s:s):unit = 
  let rec print sp ppf = function
    | SLet (id, TEmpty, e) ->
      fprintf ppf "%slet %s = %a;;@."
        sp
        id
        (print_e "") e
    | SLet (id, t, e) ->
      fprintf ppf "%slet (%s:%a) = %a;;@."
        sp
        id
        (print_t "" "") t
        (print_e "") e
    | SLetRec (id, TEmpty, e) ->
      fprintf ppf "%slet rec %s = %a;;@."
        sp
        id
        (print_e "") e
    | SLetRec (id, t, e) ->
      fprintf ppf "%slet rec (%s:%a) = %a;;@."
        sp
        id
        (print_t "" "") t
        (print_e "") e

    | SExp e ->
      fprintf ppf "%a;;\n"
        (print_e sp) e
    | SOpen s ->
      fprintf ppf "%sopen %s;;\n"
        sp
        s

(*
    | SEmpty ->
      ()


    | SRet e ->
      fprintf ppf "%sreturn %a;"
        sp
        (print_e "") e



    | SBlock ls ->
      fprintf ppf "{\n%a\n%s}"
        (print_ls "\n" (print (sp ^ "  "))) ls
        sp

    | SCon(tis, [], e) ->
      let f ppf (t,i) =
        fprintf ppf "%a %s"
          (print_t "" "") t
          i
      in
      fprintf ppf "(%a)%a"
        (print_ls ", " f) tis
        (print ("  "^sp)) e

    | SCon(tis, es, e) ->
      let f ppf (t,i) =
        fprintf ppf "%a %s"
          (print_t "" "") t
          i
      in
      fprintf ppf "(%a):%a%a" <|_6(
        print_ls<|_2(", ", f),          tis,
        print_ls<|_2(", ", print_e ""), es,
        print ("  "^ sp),               e
      )


    | SStruct (id, super, ts) ->
      let f ppf ts =
        ts |> List.iter (print_member sp ppf)
      in
      fprintf ppf "%sstruct %s%s{\n%s\n%a};\n"
        sp
        id
        (if super = "" then "" else ":" ^ super)
        sp
        f ts

    | _ -> assert false
*)
  in
    print "" ppf s

let print_prog ppf (Prog(ss)) =
  print_ls "" print_s ppf ss;
  fprintf ppf "@."

