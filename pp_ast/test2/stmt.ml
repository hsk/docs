open Syntax

open Exp
open Ty
type t = 
  | SBlock of t list
  | SIf of Exp.e * t * t
  | SEmpty
  | SExp of Exp.e
  | SRet of Exp.e
  | SFun of Ty.t * string * (Ty.t * string) list * t
  | SInclude of string
  | SLet of Ty.t * Exp.e * Exp.e
  | SStruct of string * string * (Ty.t * t) list
  | SCon of (Ty.t * string) list * Exp.e list * t
  | STrait of  string * (Ty.t * t) list
  | SImpl of string * string * t list
  | SList of t list

let rec cnve (e:e):e =
  match e with
  | EBin(e1,op,e2) -> EBin(cnve e1,op,cnve e2)
  | EPre(op,e1) -> EPre(op, cnve e1)
  | ECall(e1,es) -> ECall(cnve e1,List.map cnve es)
  | ECallM(i,e1,es) ->
    begin match cnve e1 with
    | EBin(e1,"->",fn) ->
      let ea = ECast(Ty (i^"*"),
        EArr(EBin(EVar(i^"_v"),"->",EVar "data"), [EBin(e1,".",EVar "id")])) in
      ECall(EBin(ea,"->",fn),(ECast(Ty "Class*", EPre("&",e1)))::(List.map cnve es))
    | t -> assert false
    end        
  | EArr(e1,es) -> ECall(cnve e1,List.map cnve es)
  | ECast(t,e) -> ECast(t, cnve e)
  | EInt i -> e
  | EString _ -> e
  | EVar _ -> e
  | EEmpty -> e

let rec cnvfun i i2 (t:t):t =
  match t with
  | SFun(t1,id,ls,SBlock b) ->
    SFun(
      t1,
      i^"_"^i2^"_"^id,
      (Ty "Class*","self_")::ls,

      SBlock(
        (SLet(Ty (i2 ^ "*"), EVar "self", ECast(Ty (i2 ^ "*"), EVar "self_")))::
        (List.map (cnvfun i i2) b)
      )

    )
  | t -> cnv(t)

and cnv(t:t):t =
  match t with
  | SList(ls) -> SList(List.map cnv ls)
  | STrait(id,ts) ->

    let ts =
      List.map begin function
        | (TFun(t1,ts),t) -> (TFun(t1,(Ty "Class*")::ts),t)
        | (ty,t) -> (ty,t)
      end ts
    in
    SList([
      SStruct(id,"",ts);
      (*
        Vec* Fib2_v = newVec();
      *)
      SLet(Ty "Vec*", EVar (id ^ "_v"), ECall(EVar"newVec",[]))
    ])
  | SImpl(id,id2,ss) ->
    let ls = (List.fold_right begin fun s ls ->
      match s with
      | SFun(_, name ,_,_) ->
        (SExp(EBin(EBin(EVar "impl", "->", EVar name), "=", EPre("&", EVar(id^"_"^id2^"_"^name)))))::ls
      | _ -> ls
      end ss [SRet(EVar "impl")] )
    in
    let ss = List.map (cnvfun id id2) ss in
    SList(ss@[
      SFun(Ty(id^"*"), "new"^id^"_"^id2,[],SBlock(
        (SLet(Ty(id), EPre("*", EVar "impl"), EPre("new",ECall(EVar id,[]))))::
        (SExp(ECall(EVar "setVec", [ EVar(id^"_v"); EVar (id2^"_classId"); ECast(Ty "void*",EVar "impl") ])))::
        ls
      ));
      (*
        "^id^"* "^id^"_Int_ = new"^id^"_Int();
      *)
      SLet(Ty(id^"*"), EVar (id^"_"^id2^"_"), ECall(EVar("new"^id^"_"^id2),[]))

    ])
  | SInclude _ -> t
  | SLet(t,e1,e2) -> SLet(t, cnve e1, cnve e2)
  | SStruct(v,"",tts) ->
    SList[
      SLet(Ty "int", EVar (v ^ "_classId"), ECall(EVar"Class_genId",[]));

      SStruct(v,
        "",
        (Ty "int", SExp(EVar "id")) ::
        (List.map begin fun(ty,t) ->
          let ty,t = match ty,t with
            | (Ty "", SCon(tyss,es,t1)) ->
              let id = ECall(EVar "id",[EVar(v^"_classId")]) in

              (Ty v, SCon(tyss,id::es,t1))
            | _ -> ty,t
          in
          (ty, cnv t)
        end tts))
    ]
  | SStruct(v,super,tts) ->
    SList[
      SLet(Ty "int", EVar (v ^ "_classId"), ECall(EVar"Class_genId",[]));

      SStruct(v,
        super,
        (List.map begin fun(ty,t) ->
          let ty,t = match ty,t with
            | (Ty "", SCon(tyss,es,t1)) ->
              let id = EBin(EVar "id","=",EVar(v^"_classId")) in

              (Ty v, SCon(tyss,es,SBlock[SExp(id);t1]))
            | _ -> ty,t
          in
          (ty, cnv t)
        end tts))
    ]
  | SExp(e) -> SExp(cnve e)
  | SCon(tyss,es,t1) ->
    SCon(tyss,es,t1)
  | SBlock(l) -> SBlock(List.map cnv l)
  | SEmpty -> t
  | SIf(e1,t1,t2) -> SIf(cnve e1, cnv t1, cnv t2)
  | SRet(e) -> SRet(cnve e)
  | SFun(ty, id, tyis, t2) -> SFun(ty, id, tyis, cnv t2)

and print (t:t):string = 
  let rec print sp e =
  match e with
  | SBlock ls -> Printf.sprintf "{\n%s\n%s}" (print_ls ~sep:"\n" (print (sp^"  "))ls) sp
  | SIf(e1,e2,SEmpty) ->
    Printf.sprintf "%sif (%s)%s%s"
      sp
      (Exp.print "" e1)
      (print2 sp e2 "\n")
      sp
  | SIf(e1,e2,e3) ->
    Printf.sprintf "%sif (%s)%s%selse%s"
      sp
      (Exp.print "" e1)
      (print2 sp e2 "\n")
      sp
      (print2 sp e3 "")
  | SFun (t, id, ts, e2) ->
    Printf.sprintf "%s %s(%s)%s"
      (Ty.print "" t)
      id
      (print_ls (fun (t,a)-> Printf.sprintf "%s %s" (Ty.print "" t) a) ts ~sep:", " )
      (print2 sp e2 "\n")
  | SStruct (id, super, ts) ->
    Printf.sprintf "%sstruct %s%s{\n%s\n%s};\n"
      sp
      id
      (if super = "" then "" else ":" ^ super)
      sp
      (List.fold_left (print_mem sp) "" ts)
  | SList ls -> List.fold_left(fun r t -> r ^ (print sp t) ^ "\n") "" ls  
  | SEmpty -> ""
  | SExp e -> Exp.print sp e ^ ";"
  | SRet e -> sp ^ "return " ^ Exp.print "" e ^ ";"
  | SInclude s -> "#include " ^ s
  | SLet (t, id, Exp.EEmpty) -> Printf.sprintf "%s%s %s" sp (Ty.print "" t) (Exp.print "" id) ^ ";"
  | SLet (t, id, e) -> Printf.sprintf "%s%s %s = %s" sp (Ty.print "" t) (Exp.print "" id) (Exp.print "" e) ^ ";"
  | SCon(tis,[],e) ->
    Printf.sprintf "(%s)%s"
      (print_ls (fun(t,i)->Ty.print "" t ^ " "^i) tis ~sep:", ") (print ("  "^sp) e)
  | SCon(tis,es,e) ->
    Printf.sprintf "(%s):%s%s"
      (print_ls (fun(t,i)->Ty.print "" t ^ " "^i) tis ~sep:", ")
      (print_ls (Exp.print "") es ~sep:", ") (print ("  "^ sp) e)
  | _ -> assert false
  and print2 sp e ed = 
    match e with
    | SBlock ls -> " " ^ print sp e ^ (if ed <> "" then " " else "")
    | _ -> "\n" ^ print (sp^"  ") e ^ ed
  and print_mem sp r (t,s) =
    (match t,s with
    | Ty.TFun(_,_),SExp(s) ->
      Printf.sprintf "%s%s;\n"
        r
        (Ty.print ("  "^sp) t ~pp:(Exp.print "" s))
    | _ ->
      Printf.sprintf "%s%s %s\n"
        r
        (Ty.print ("  "^sp) t)
        (print "" s)
  )
  in print "" t

let prints (ts:t list): string =
  print_ls ~sep:"\n" print ts

