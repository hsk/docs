module Syntax = struct
  type e =
    | EInt of int
    | EAdd of e * e
    | EMul of e * e
    | ESub of e * e
    | EDiv of e * e
end

let counter = ref 0

let gen_id () =
  incr counter;
  "tmp" ^ string_of_int !counter

module KNormal = struct
  type e =
    | EInt of int
    | EAdd of string * string
    | EMul of string * string
    | ESub of string * string
    | EDiv of string * string
    | ELet of string * e * e
    | EUnit

  let insert_let e k = (* letを挿入する補助関数 *)
        let x = gen_id() in
        let e' = k x in
        ELet(x, e, e')

  let rec f e =
    match e with
    | Syntax.EInt(i) -> EInt(i)
    | Syntax.EAdd(e1,e2) ->
      insert_let (f e1)
        (fun x -> insert_let (f e2)
            (fun y -> EAdd(x, y)))
    | Syntax.EMul(e1,e2) ->
      insert_let (f e1)
        (fun x -> insert_let (f e2)
            (fun y -> EAdd(x, y)))
    | Syntax.ESub(e1,e2) ->
      insert_let (f e1)
        (fun x -> insert_let (f e2)
            (fun y -> EAdd(x, y)))
    | Syntax.EDiv(e1,e2) ->
      insert_let (f e1)
        (fun x -> insert_let (f e2)
            (fun y -> EAdd(x, y)))

end

module Emit = struct
  open KNormal

  let rec p fp e =
    match e with
    | EInt(i) -> Format.fprintf fp "(%d)" i
    | EAdd(e1,e2) -> Format.fprintf fp "(frame[%s]->intv+frame[%s]->intv)" e1 e2
    | EMul(e1,e2) -> Format.fprintf fp "(frame[%s]->intv*frame[%s]->intv)" e1 e2
    | ESub(e1,e2) -> Format.fprintf fp "(frame[%s]->intv-frame[%s]->intv)" e1 e2
    | EDiv(e1,e2) -> Format.fprintf fp "(frame[%s]->intv/frame[%s]->intv)" e1 e2
    | ELet(x,e1, (ELet(_,_,_) as e2)) -> Format.fprintf fp "  frame[%s]=gc_alloc_int(%a);\n%a\n" x p e1 p e2 
    | ELet(x,e1,e2) -> Format.fprintf fp "  frame[%s]=gc_alloc_int(%a);\n  printf(\"%%d\\n\", %a);" x p e1 p e2
    | EUnit -> ()
  let f fp e =
    let ls =
      let rec loop ls = function
      | ELet(x, e1, e2) -> loop (loop (x::ls) e1) e2
      | _ -> ls
      in loop [] e
    in
    Format.fprintf fp "#include \"gc.h\"\n";
    Format.fprintf fp "#include <stdio.h>\n";
    Format.fprintf fp "\n";
    Format.fprintf fp "void _main(){\n";
    Format.fprintf fp "  enum {FRAME_START, FRAME_SIZE, %s, FRAME_END};\n" (String.concat "," ls);
    Format.fprintf fp "  ENTER_FRAME_ENUM();\n";
    p fp e;
(*    Format.fprintf fp "  printf(\"%%d\\n\", frame[%s]->ints[0]);\n" s;*)
    Format.fprintf fp "  LEAVE_FRAME();\n";
    Format.fprintf fp "}\n";
    Format.fprintf fp "\n";
    Format.fprintf fp "int main() {\n";
    Format.fprintf fp "  gc_init();\n";
    Format.fprintf fp "  _main();\n";
    Format.fprintf fp "  gc_free();\n";
    Format.fprintf fp "  return 0;\n";
    Format.fprintf fp "}\n"
end

open Syntax


let _ =
  let k = KNormal.f(EAdd(EInt 1,EInt 2)) in
  Emit.f Format.std_formatter k;

