open Syntax
open Format

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

and print ppf (t:t):unit = 
  let rec print sp ppf = function
  | SBlock ls ->
    fprintf ppf "{\n%a\n%s}" (print_ls "\n" (print (sp^"  "))) ls sp
  | SIf(e1,e2,SEmpty) ->
    fprintf ppf "%sif (%a)%a%s"
      sp
      (Exp.print "") e1
      (print2 sp "\n") e2
      sp
  | SIf(e1,e2,e3) ->
    fprintf ppf "%sif (%a)%a%selse%a"
      sp
      (Exp.print "" ) e1
      (print2 sp "\n") e2 
      sp
      (print2 sp "") e3 
  | SFun (t, id, ts, e2) ->
    fprintf ppf "%a %s(%a)%a"
      (Ty.print "" "") t
      id
      (print_ls ", " (fun ppf (t,a) -> fprintf ppf "%a %s" (Ty.print "" "") t a)) ts 
      (print2 sp "\n") e2
  | SStruct (id, super, ts) ->
    fprintf ppf "%sstruct %s%s{\n%s\n%a};\n"
      sp
      id
      (if super = "" then "" else ":" ^ super)
      sp
      (fun ppf ts -> List.iter (print_mem sp ppf) ts) ts
  | SList ls ->
    List.iter(fun t -> fprintf ppf "%a@." (print sp) t) ls
  | SEmpty -> ()
  | SExp e -> fprintf ppf "%a;" (Exp.print sp) e
  | SRet e -> fprintf ppf "%sreturn %a;" sp (Exp.print "") e
  | SInclude s -> fprintf ppf "#include %s" s
  | SLet (t, id, Exp.EEmpty) ->
    fprintf ppf "%s%a %a;"
      sp
      (Ty.print "" "") t
      (Exp.print "") id
  | SLet (t, id, e) ->
    fprintf ppf "%s%a %a = %a;"
      sp
      (Ty.print "" "") t
      (Exp.print "") id
      (Exp.print "") e
  | SCon(tis,[],e) ->
    fprintf ppf "(%a)%a"
      (print_ls ", " (fun ppf (t,i)-> fprintf ppf "%a %s" (Ty.print "" "") t i)) tis
      (print ("  "^sp)) e
  | SCon(tis,es,e) ->
    fprintf ppf "(%a):%a%a"
      (print_ls ", " (fun ppf (t,i)-> fprintf ppf "%a %s" (Ty.print "" "") t i)) tis
      (print_ls ", " (Exp.print "")) es 
      (print ("  "^ sp)) e
  | _ -> assert false
  and print2 sp ed ppf e =  
    match e with
    | SBlock ls ->
      fprintf ppf " %a%s"
        (print sp) e
        (if ed <> "" then " " else "")
    | _ ->
      fprintf ppf "\n%a%s"
        (print (sp^"  ")) e
        ed
  and print_mem sp ppf = function
    | (Ty.TFun(_,_) as t),SExp(s) ->
      let buf = Buffer.create 1024 in
      let formatter = (Format.formatter_of_buffer buf) in 
      Exp.print "" formatter s;
      fprintf formatter "@?";
      let ss = Buffer.contents buf in 
      fprintf ppf "%a;\n"
        (Ty.print ss ("  "^sp)) t 
    | (t,s) ->
      fprintf ppf "%a %a\n"
        (Ty.print "" ("  "^sp)) t
        (print "") s
  in print "" ppf t

let prints ppf (ts:t list) =
  print_ls "\n" print ppf  ts

