type e =
  | EInt of int
  | EAdd of e * e
  | EMul of e * e
  | ESub of e * e
  | EDiv of e * e

let rec p fp e =
  match e with
  | EInt(i) -> Format.fprintf fp "(%d)" i
  | EAdd(e1,e2) -> Format.fprintf fp "(%a+%a)" p e1 p e2
  | EMul(e1,e2) -> Format.fprintf fp "(%a*%a)" p e1 p e2
  | ESub(e1,e2) -> Format.fprintf fp "(%a-%a)" p e1 p e2
  | EDiv(e1,e2) -> Format.fprintf fp "(%a/%a)" p e1 p e2

let comp fp e =
  Format.fprintf fp "#include \"gc.h\"\n";
  Format.fprintf fp "#include <stdio.h>\n";
  Format.fprintf fp "\n";
  Format.fprintf fp "void _main(){\n";
  Format.fprintf fp "  enum {FRAME_START, FRAME_SIZE, A, FRAME_END};\n";
  Format.fprintf fp "  ENTER_FRAME_ENUM();\n";
  Format.fprintf fp "  frame[A]= gc_alloc_int(%a);\n" p e;
  Format.fprintf fp "  printf(\"%%d\\n\", frame[A]->ints[0]);\n";
  Format.fprintf fp "  LEAVE_FRAME();\n";
  Format.fprintf fp "}\n";
  Format.fprintf fp "\n";
  Format.fprintf fp "int main() {\n";
  Format.fprintf fp "  gc_init();\n";
  Format.fprintf fp "  _main();\n";
  Format.fprintf fp "  gc_free();\n";
  Format.fprintf fp "  return 0;\n";
  Format.fprintf fp "}\n"

let _ =
  comp Format.std_formatter (EAdd(EInt 1,EInt 2));

