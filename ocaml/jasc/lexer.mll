{
  open Parser
       
  let get = Lexing.lexeme

  let keyword_table = Hashtbl.create 53
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
      [
        (* reserved_words used in Jasmin directives *)
        "field", FIELD;
        "from", FROM;
        "method", METHOD;
        "to", TO;
        "is", IS;
        "using", USING;
        "signature", SIGNATURE;
        "stack", STACK;
        "offset", OFFSET;
        "locals", LOCALS;
        "use", USE;
        "inner", INNER;
        "outer", OUTER;
        "class", CLASS;
        "visible", VISIBLE;
        "invisible", INVISIBLE;
        "visibleparam", VISIBLEPARAM;
        "invisibleparam", INVISIBLEPARAM;

        (* Special-case instructions *)
        "tableswitch", TABLESWITCH;
        "lookupswitch", LOOKUPSWITCH;
        "default", DEFAULT;

        (* Access flags *)
        "public", PUBLIC;
        "private", PRIVATE;
        "protected", PROTECTED;
        "static", STATIC;
        "final", FINAL;
        "synchronized", SYNCHRONIZED;
        "volatile", VOLATILE;
        "transient", TRANSIENT;
        "native", NATIVE;
        "interface", INTERFACE;
        "abstract", ABSTRACT;

        "annotation", ANNOTATION;
        "enum", ENUM;
        "bridge", BRIDGE;
        "varargs", VARARGS;
        "fpstrict", STRICT;
        "synthetic", SYNTHETIC;
 ]
  let dbg a = (print_endline a)
  let dbg a = ()
}
(*******************************************************************
 * Helpers                                                         *
 *******************************************************************)

let latin1_input_character = ['\000'- '\255']
let ht = '\t'
let lf = '\n'
let ff = '\012'
let cr = '\r'
let sp = ' '

let line_terminator = lf | cr | cr lf 
let input_character = ['\000'-'\255'] # ['\r' '\n'] (* # (lf | cr) *)

let not_star_not_newline = (['\000'-'\255'] # ['\r''\n''*'])
let not_star_not_slash_not_newline = (['\000'-'\255'] # ['\r''\n''*''/'])

let digit = ['0'-'9']
let non_zero_digit = ['1'-'9']

let decimal_numeral = '0' | non_zero_digit digit*

let latin1_letter =
       ['A'-'Z'] | ['a'-'z'] | ['\170'-'\170'] | ['\181'-'\181'] |
       ['\186'-'\186'] | ['\192'-'\214'] | ['\216'-'\246'] | ['\248'-'\255']

let java_letter = latin1_letter | '$' | '_'
let java_letter_or_digit = latin1_letter | digit | '$' | '_'

let string_character = (['\000'-'\255'] # ['\r' '\n' '"' '\\'])
(*******************************************************************
 * Tokens                                                          *
 *******************************************************************)

rule token = parse
  | eof    { EOF }
  
  (* Whitespace *)
  | (sp | ht | ff)                  { token lexbuf }
  | (line_terminator+ (sp | ht | ff)* (";" input_character*)? (sp | ht | ff)*)+                { dbg("SEP"); SEP }
  | ";" input_character*
                                    { dbg(Lexing.lexeme lexbuf); token lexbuf } (* end_of_line_comment *)



  | "aaload" { dbg(Lexing.lexeme lexbuf); Insn("aaload", "") }
  | "aastore" { dbg(Lexing.lexeme lexbuf); Insn("aastore", "") }
  | "aconst_null" { dbg(Lexing.lexeme lexbuf); Insn("aconst_null", "") }
  | "aload" { dbg(Lexing.lexeme lexbuf); Insn("aload", "i") }
  | "aload_w" { dbg(Lexing.lexeme lexbuf); Insn("aload", "I") }
  | "aload_0" { dbg(Lexing.lexeme lexbuf); Insn("aload_0", "") }
  | "aload_1" { dbg(Lexing.lexeme lexbuf); Insn("aload_1", "") }
  | "aload_2" { dbg(Lexing.lexeme lexbuf); Insn("aload_2", "") }
  | "aload_3" { dbg(Lexing.lexeme lexbuf); Insn("aload_3", "") }
  | "anewarray" { dbg(Lexing.lexeme lexbuf); Insn("anewarray", "class") }
  | "areturn" { dbg(Lexing.lexeme lexbuf); Insn("areturn", "") }
  | "arraylength" { dbg(Lexing.lexeme lexbuf); Insn("arraylength", "") }
  | "astore" { dbg(Lexing.lexeme lexbuf); Insn("astore", "i") }
  | "astore_w" { dbg(Lexing.lexeme lexbuf); Insn("astore", "I") }
  | "astore_0" { dbg(Lexing.lexeme lexbuf); Insn("astore_0", "") }
  | "astore_1" { dbg(Lexing.lexeme lexbuf); Insn("astore_1", "") }
  | "astore_2" { dbg(Lexing.lexeme lexbuf); Insn("astore_2", "") }
  | "astore_3" { dbg(Lexing.lexeme lexbuf); Insn("astore_3", "") }
  | "athrow" { dbg(Lexing.lexeme lexbuf); Insn("athrow", "") }
  | "baload" { dbg(Lexing.lexeme lexbuf); Insn("baload", "") }
  | "bastore" { dbg(Lexing.lexeme lexbuf); Insn("bastore", "") }
  | "bipush" { dbg(Lexing.lexeme lexbuf); Insn("bipush", "i") }
  | "breakpoint" { dbg(Lexing.lexeme lexbuf); Insn("breakpoint", "") }
  | "caload" { dbg(Lexing.lexeme lexbuf); Insn("caload", "") }
  | "castore" { dbg(Lexing.lexeme lexbuf); Insn("castore", "") }
  | "checkcast" { dbg(Lexing.lexeme lexbuf); Insn("checkcast", "class") }
  | "d2f" { dbg(Lexing.lexeme lexbuf); Insn("d2f", "") }
  | "d2i" { dbg(Lexing.lexeme lexbuf); Insn("d2i", "") }
  | "d2l" { dbg(Lexing.lexeme lexbuf); Insn("d2l", "") }
  | "dadd" { dbg(Lexing.lexeme lexbuf); Insn("dadd", "") }
  | "daload" { dbg(Lexing.lexeme lexbuf); Insn("daload", "") }
  | "dastore" { dbg(Lexing.lexeme lexbuf); Insn("dastore", "") }
  | "dcmpg" { dbg(Lexing.lexeme lexbuf); Insn("dcmpg", "") }
  | "dcmpl" { dbg(Lexing.lexeme lexbuf); Insn("dcmpl", "") }
  | "dconst_0" { dbg(Lexing.lexeme lexbuf); Insn("dconst_0", "") }
  | "dconst_1" { dbg(Lexing.lexeme lexbuf); Insn("dconst_1", "") }
  | "ddiv" { dbg(Lexing.lexeme lexbuf); Insn("ddiv", "") }
  | "dload" { dbg(Lexing.lexeme lexbuf); Insn("dload", "i") }
  | "dload_w" { dbg(Lexing.lexeme lexbuf); Insn("dload", "I") }
  | "dload_0" { dbg(Lexing.lexeme lexbuf); Insn("dload_0", "") }
  | "dload_1" { dbg(Lexing.lexeme lexbuf); Insn("dload_1", "") }
  | "dload_2" { dbg(Lexing.lexeme lexbuf); Insn("dload_2", "") }
  | "dload_3" { dbg(Lexing.lexeme lexbuf); Insn("dload_3", "") }
  | "dmul" { dbg(Lexing.lexeme lexbuf); Insn("dmul", "") }

  (* added this synonym javalib *)
  | "dmult" { dbg(Lexing.lexeme lexbuf); Insn("dmul", "") }
  | "dneg" { dbg(Lexing.lexeme lexbuf); Insn("dneg", "") }
  | "drem" { dbg(Lexing.lexeme lexbuf); Insn("drem", "") }
  | "dreturn" { dbg(Lexing.lexeme lexbuf); Insn("dreturn", "") }
  | "dstore" { dbg(Lexing.lexeme lexbuf); Insn("dstore", "i") }
  | "dstore_w" { dbg(Lexing.lexeme lexbuf); Insn("dstore", "I") }
  | "dstore_0" { dbg(Lexing.lexeme lexbuf); Insn("dstore_0", "") }
  | "dstore_1" { dbg(Lexing.lexeme lexbuf); Insn("dstore_1", "") }
  | "dstore_2" { dbg(Lexing.lexeme lexbuf); Insn("dstore_2", "") }
  | "dstore_3" { dbg(Lexing.lexeme lexbuf); Insn("dstore_3", "") }
  | "dsub" { dbg(Lexing.lexeme lexbuf); Insn("dsub", "") }
  | "dup" { dbg(Lexing.lexeme lexbuf); Insn("dup", "") }
  | "dup2" { dbg(Lexing.lexeme lexbuf); Insn("dup2", "") }
  | "dup2_x1" { dbg(Lexing.lexeme lexbuf); Insn("dup2_x1", "") }
  | "dup2_x2" { dbg(Lexing.lexeme lexbuf); Insn("dup2_x2", "") }
  | "dup_x1" { dbg(Lexing.lexeme lexbuf); Insn("dup_x1", "") }
  | "dup_x2" { dbg(Lexing.lexeme lexbuf); Insn("dup_x2", "") }
  | "f2d" { dbg(Lexing.lexeme lexbuf); Insn("f2d", "") }
  | "f2i" { dbg(Lexing.lexeme lexbuf); Insn("f2i", "") }
  | "f2l" { dbg(Lexing.lexeme lexbuf); Insn("f2l", "") }
  | "fadd" { dbg(Lexing.lexeme lexbuf); Insn("fadd", "") }
  | "faload" { dbg(Lexing.lexeme lexbuf); Insn("faload", "") }
  | "fastore" { dbg(Lexing.lexeme lexbuf); Insn("fastore", "") }
  | "fcmpg" { dbg(Lexing.lexeme lexbuf); Insn("fcmpg", "") }
  | "fcmpl" { dbg(Lexing.lexeme lexbuf); Insn("fcmpl", "") }
  | "fconst_0" { dbg(Lexing.lexeme lexbuf); Insn("fconst_0", "") }
  | "fconst_1" { dbg(Lexing.lexeme lexbuf); Insn("fconst_1", "") }
  | "fconst_2" { dbg(Lexing.lexeme lexbuf); Insn("fconst_2", "") }
  | "fdiv" { dbg(Lexing.lexeme lexbuf); Insn("fdiv", "") }
  | "fload" { dbg(Lexing.lexeme lexbuf); Insn("fload", "i") }
  | "fload_w" { dbg(Lexing.lexeme lexbuf); Insn("fload", "I") }
  | "fload_0" { dbg(Lexing.lexeme lexbuf); Insn("fload_0", "") }
  | "fload_1" { dbg(Lexing.lexeme lexbuf); Insn("fload_1", "") }
  | "fload_2" { dbg(Lexing.lexeme lexbuf); Insn("fload_2", "") }
  | "fload_3" { dbg(Lexing.lexeme lexbuf); Insn("fload_3", "") }
  | "fmul" { dbg(Lexing.lexeme lexbuf); Insn("fmul", "") }

  (* added this synonym javalib *)
  | "fmult" { dbg(Lexing.lexeme lexbuf); Insn("fmul", "") }
  | "fneg" { dbg(Lexing.lexeme lexbuf); Insn("fneg", "") }
  | "frem" { dbg(Lexing.lexeme lexbuf); Insn("frem", "") }
  | "freturn" { dbg(Lexing.lexeme lexbuf); Insn("freturn", "") }
  | "fstore" { dbg(Lexing.lexeme lexbuf); Insn("fstore", "i") }
  | "fstore_w" { dbg(Lexing.lexeme lexbuf); Insn("fstore", "I") }
  | "fstore_0" { dbg(Lexing.lexeme lexbuf); Insn("fstore_0", "") }
  | "fstore_1" { dbg(Lexing.lexeme lexbuf); Insn("fstore_1", "") }
  | "fstore_2" { dbg(Lexing.lexeme lexbuf); Insn("fstore_2", "") }
  | "fstore_3" { dbg(Lexing.lexeme lexbuf); Insn("fstore_3", "") }
  | "fsub" { dbg(Lexing.lexeme lexbuf); Insn("fsub", "") }
  | "getfield" { dbg(Lexing.lexeme lexbuf); Insn("getfield", "field") }
  | "getstatic" { dbg(Lexing.lexeme lexbuf); Insn("getstatic", "field") }
  | "goto" { dbg(Lexing.lexeme lexbuf); Insn("goto", "label") }
  | "goto_w" { dbg(Lexing.lexeme lexbuf); Insn("goto_w", "label") }
  | "i2d" { dbg(Lexing.lexeme lexbuf); Insn("i2d", "") }
  | "i2f" { dbg(Lexing.lexeme lexbuf); Insn("i2f", "") }
  | "i2l" { dbg(Lexing.lexeme lexbuf); Insn("i2l", "") }
  | "iadd" { dbg(Lexing.lexeme lexbuf); Insn("iadd", "") }
  | "iaload" { dbg(Lexing.lexeme lexbuf); Insn("iaload", "") }
  | "iand" { dbg(Lexing.lexeme lexbuf); Insn("iand", "") }
  | "iastore" { dbg(Lexing.lexeme lexbuf); Insn("iastore", "") }
  | "iconst_0" { dbg(Lexing.lexeme lexbuf); Insn("iconst_0", "") }
  | "iconst_1" { dbg(Lexing.lexeme lexbuf); Insn("iconst_1", "") }
  | "iconst_2" { dbg(Lexing.lexeme lexbuf); Insn("iconst_2", "") }
  | "iconst_3" { dbg(Lexing.lexeme lexbuf); Insn("iconst_3", "") }
  | "iconst_4" { dbg(Lexing.lexeme lexbuf); Insn("iconst_4", "") }
  | "iconst_5" { dbg(Lexing.lexeme lexbuf); Insn("iconst_5", "") }
  | "iconst_m1" { dbg(Lexing.lexeme lexbuf); Insn("iconst_m1", "") }
  | "idiv" { dbg(Lexing.lexeme lexbuf); Insn("idiv", "") }
  | "if_acmpeq" { dbg(Lexing.lexeme lexbuf); Insn("if_acmpeq", "label") }
  | "if_acmpne" { dbg(Lexing.lexeme lexbuf); Insn("if_acmpne", "label") }
  | "if_icmpeq" { dbg(Lexing.lexeme lexbuf); Insn("if_icmpeq", "label") }
  | "if_icmpge" { dbg(Lexing.lexeme lexbuf); Insn("if_icmpge", "label") }
  | "if_icmpgt" { dbg(Lexing.lexeme lexbuf); Insn("if_icmpgt", "label") }
  | "if_icmple" { dbg(Lexing.lexeme lexbuf); Insn("if_icmple", "label") }
  | "if_icmplt" { dbg(Lexing.lexeme lexbuf); Insn("if_icmplt", "label") }
  | "if_icmpne" { dbg(Lexing.lexeme lexbuf); Insn("if_icmpne", "label") }
  | "ifeq" { dbg(Lexing.lexeme lexbuf); Insn("ifeq", "label") }
  | "ifge" { dbg(Lexing.lexeme lexbuf); Insn("ifge", "label") }
  | "ifgt" { dbg(Lexing.lexeme lexbuf); Insn("ifgt", "label") }
  | "ifle" { dbg(Lexing.lexeme lexbuf); Insn("ifle", "label") }
  | "iflt" { dbg(Lexing.lexeme lexbuf); Insn("iflt", "label") }
  | "ifne" { dbg(Lexing.lexeme lexbuf); Insn("ifne", "label") }
  | "ifnonnull" { dbg(Lexing.lexeme lexbuf); Insn("ifnonnull", "label") }
  | "ifnull" { dbg(Lexing.lexeme lexbuf); Insn("ifnull", "label") }
  | "iinc" { dbg(Lexing.lexeme lexbuf); Insn("iinc", "ii") }
  | "iinc_w" { dbg(Lexing.lexeme lexbuf); Insn("iinc", "Ii") }
  | "iload" { dbg(Lexing.lexeme lexbuf); Insn("iload", "i") }
  | "iload_w" { dbg(Lexing.lexeme lexbuf); Insn("iload", "I") }
  | "iload_0" { dbg(Lexing.lexeme lexbuf); Insn("iload_0", "") }
  | "iload_1" { dbg(Lexing.lexeme lexbuf); Insn("iload_1", "") }
  | "iload_2" { dbg(Lexing.lexeme lexbuf); Insn("iload_2", "") }
  | "iload_3" { dbg(Lexing.lexeme lexbuf); Insn("iload_3", "") }

  (* added this synonym javalib *)
  | "ilor" { dbg(Lexing.lexeme lexbuf); Insn("lor", "") }
  | "imul" { dbg(Lexing.lexeme lexbuf); Insn("imul", "") }

  (* added this synonym javalib *)
  | "imult" { dbg(Lexing.lexeme lexbuf); Insn("imul", "") }
  | "ineg" { dbg(Lexing.lexeme lexbuf); Insn("ineg", "") }
  | "instanceof" { dbg(Lexing.lexeme lexbuf); Insn("instanceof", "class") }
  | "int2byte" { dbg(Lexing.lexeme lexbuf); Insn("int2byte", "") }
  | "int2char" { dbg(Lexing.lexeme lexbuf); Insn("int2char", "") }
  | "int2short" { dbg(Lexing.lexeme lexbuf); Insn("int2short", "") }
  
  (* added this synonym *)
  | "i2b" { dbg(Lexing.lexeme lexbuf); Insn("int2byte", "") }
  
  (* added this synonym *)
  | "i2c" { dbg(Lexing.lexeme lexbuf); Insn("int2char", "") }
  
  (* added this synonym *)
  | "i2s" { dbg(Lexing.lexeme lexbuf); Insn("int2short", "") }
  | "invokedynamic" { dbg(Lexing.lexeme lexbuf); Insn("invokedynamic", "method") }
  | "invokeinterface" { dbg(Lexing.lexeme lexbuf); Insn("invokeinterface", "interface") }
  | "invokenonvirtual" { dbg(Lexing.lexeme lexbuf); Insn("invokenonvirtual", "method") }
  
  (* added this synonym *)
  | "invokespecial" { dbg(Lexing.lexeme lexbuf); Insn("invokenonvirtual", "method") }
  | "invokestatic" { dbg(Lexing.lexeme lexbuf); Insn("invokestatic", "method") }
  | "invokevirtual" { dbg(Lexing.lexeme lexbuf); Insn("invokevirtual", "method") }
  | "ior" { dbg(Lexing.lexeme lexbuf); Insn("ior", "") }
  | "irem" { dbg(Lexing.lexeme lexbuf); Insn("irem", "") }
  | "ireturn" { dbg(Lexing.lexeme lexbuf); Insn("ireturn", "") }
  | "ishl" { dbg(Lexing.lexeme lexbuf); Insn("ishl", "") }
  | "ishr" { dbg(Lexing.lexeme lexbuf); Insn("ishr", "") }
  | "istore" { dbg(Lexing.lexeme lexbuf); Insn("istore", "i") }
  | "istore_w" { dbg(Lexing.lexeme lexbuf); Insn("istore", "I") }
  | "istore_0" { dbg(Lexing.lexeme lexbuf); Insn("istore_0", "") }
  | "istore_1" { dbg(Lexing.lexeme lexbuf); Insn("istore_1", "") }
  | "istore_2" { dbg(Lexing.lexeme lexbuf); Insn("istore_2", "") }
  | "istore_3" { dbg(Lexing.lexeme lexbuf); Insn("istore_3", "") }
  | "isub" { dbg(Lexing.lexeme lexbuf); Insn("isub", "") }
  | "iushr" { dbg(Lexing.lexeme lexbuf); Insn("iushr", "") }
  | "ixor" { dbg(Lexing.lexeme lexbuf); Insn("ixor", "") }
  | "jsr" { dbg(Lexing.lexeme lexbuf); Insn("jsr", "label") }
  | "jsr_w" { dbg(Lexing.lexeme lexbuf); Insn("jsr_w", "label") }
  | "l2d" { dbg(Lexing.lexeme lexbuf); Insn("l2d", "") }
  | "l2f" { dbg(Lexing.lexeme lexbuf); Insn("l2f", "") }
  | "l2i" { dbg(Lexing.lexeme lexbuf); Insn("l2i", "") }
  | "ladd" { dbg(Lexing.lexeme lexbuf); Insn("ladd", "") }
  | "laload" { dbg(Lexing.lexeme lexbuf); Insn("laload", "") }
  | "land" { dbg(Lexing.lexeme lexbuf); Insn("land", "") }
  | "lastore" { dbg(Lexing.lexeme lexbuf); Insn("lastore", "") }
  | "lcmp" { dbg(Lexing.lexeme lexbuf); Insn("lcmp", "") }
  | "lconst_0" { dbg(Lexing.lexeme lexbuf); Insn("lconst_0", "") }
  | "lconst_1" { dbg(Lexing.lexeme lexbuf); Insn("lconst_1", "") }
  | "ldc" { dbg(Lexing.lexeme lexbuf); Insn("ldc", "constant") }
  | "ldc_w" { dbg(Lexing.lexeme lexbuf); Insn("ldc_w", "constant") }
  | "ldc2_w" { dbg(Lexing.lexeme lexbuf); Insn("ldc2_w", "bigconstant") }
  | "ldiv" { dbg(Lexing.lexeme lexbuf); Insn("ldiv", "") }
  | "lload" { dbg(Lexing.lexeme lexbuf); Insn("lload", "i") }
  | "lload_w" { dbg(Lexing.lexeme lexbuf); Insn("lload", "I") }
  | "lload_0" { dbg(Lexing.lexeme lexbuf); Insn("lload_0", "") }
  | "lload_1" { dbg(Lexing.lexeme lexbuf); Insn("lload_1", "") }
  | "lload_2" { dbg(Lexing.lexeme lexbuf); Insn("lload_2", "") }
  | "lload_3" { dbg(Lexing.lexeme lexbuf); Insn("lload_3", "") }
  | "lmul" { dbg(Lexing.lexeme lexbuf); Insn("lmul", "") }

  (* added this synonym javalib *)
  | "lmult" { dbg(Lexing.lexeme lexbuf); Insn("lmul", "") }
  | "lneg" { dbg(Lexing.lexeme lexbuf); Insn("lneg", "") }

  (*| "lookupswitch" { dbg(Lexing.lexeme lexbuf); Insn("lookupswitch", "switch") }*)
  | "lor" { dbg(Lexing.lexeme lexbuf); Insn("lor", "") }
  | "lrem" { dbg(Lexing.lexeme lexbuf); Insn("lrem", "") }
  | "lreturn" { dbg(Lexing.lexeme lexbuf); Insn("lreturn", "") }
  | "lshl" { dbg(Lexing.lexeme lexbuf); Insn("lshl", "") }
  | "lshr" { dbg(Lexing.lexeme lexbuf); Insn("lshr", "") }
  | "lstore" { dbg(Lexing.lexeme lexbuf); Insn("lstore", "i") }
  | "lstore_w" { dbg(Lexing.lexeme lexbuf); Insn("lstore", "I") }
  | "lstore_0" { dbg(Lexing.lexeme lexbuf); Insn("lstore_0", "") }
  | "lstore_1" { dbg(Lexing.lexeme lexbuf); Insn("lstore_1", "") }
  | "lstore_2" { dbg(Lexing.lexeme lexbuf); Insn("lstore_2", "") }
  | "lstore_3" { dbg(Lexing.lexeme lexbuf); Insn("lstore_3", "") }
  | "lsub" { dbg(Lexing.lexeme lexbuf); Insn("lsub", "") }
  | "lushr" { dbg(Lexing.lexeme lexbuf); Insn("lushr", "") }
  | "lxor" { dbg(Lexing.lexeme lexbuf); Insn("lxor", "") }
  | "monitorenter" { dbg(Lexing.lexeme lexbuf); Insn("monitorenter", "") }
  | "monitorexit" { dbg(Lexing.lexeme lexbuf); Insn("monitorexit", "") }
  | "multianewarray" { dbg(Lexing.lexeme lexbuf); Insn("multianewarray", "marray") }
  | "new" { dbg(Lexing.lexeme lexbuf); Insn("new", "class") }
  | "newarray" { dbg(Lexing.lexeme lexbuf); Insn("newarray", "atype") }
  | "nop" { dbg(Lexing.lexeme lexbuf); Insn("nop", "") }
  | "pop" { dbg(Lexing.lexeme lexbuf); Insn("pop", "") }
  | "pop2" { dbg(Lexing.lexeme lexbuf); Insn("pop2", "") }
  | "putfield" { dbg(Lexing.lexeme lexbuf); Insn("putfield", "field") }
  | "putstatic" { dbg(Lexing.lexeme lexbuf); Insn("putstatic", "field") }
  | "ret" { dbg(Lexing.lexeme lexbuf); Insn("ret", "i") }
  | "ret_w" { dbg(Lexing.lexeme lexbuf); Insn("ret", "I") }
  | "return" { dbg(Lexing.lexeme lexbuf); Insn("return", "") }
  | "saload" { dbg(Lexing.lexeme lexbuf); Insn("saload", "") }
  | "sastore" { dbg(Lexing.lexeme lexbuf); Insn("sastore", "") }
  | "sipush" { dbg(Lexing.lexeme lexbuf); Insn("sipush", "i") }
  | "swap" { dbg(Lexing.lexeme lexbuf); Insn("swap", "") }

  (*| "tableswitch" { Insn("tableswitch", "switch") }*)

  (* Jasmin directives *)
  | ".annotation" { dbg(Lexing.lexeme lexbuf); DANNOTATION }
  | ".attribute" { dbg(Lexing.lexeme lexbuf); DATTRIBUTE }
  | ".bytecode" { dbg(Lexing.lexeme lexbuf); DBYTECODE }
  | ".catch" { dbg(Lexing.lexeme lexbuf); DCATCH }
  | ".class" { dbg(Lexing.lexeme lexbuf); DCLASS }
  | ".deprecated" { dbg(Lexing.lexeme lexbuf); DDEPRECATED }
  | ".end" { dbg(Lexing.lexeme lexbuf); DEND }
  | ".field" { dbg(Lexing.lexeme lexbuf); DFIELD }
  | ".implements" { dbg(Lexing.lexeme lexbuf); DIMPLEMENTS }
  | ".inner" { dbg(Lexing.lexeme lexbuf); DINNER }
  | ".interface" { dbg(Lexing.lexeme lexbuf); DINTERFACE }
  | ".limit" { dbg(Lexing.lexeme lexbuf); DLIMIT }
  | ".line" { dbg(Lexing.lexeme lexbuf); DLINE }
  | ".method" { dbg(Lexing.lexeme lexbuf); DMETHOD }
  | ".set" { dbg(Lexing.lexeme lexbuf); DSET }
  | ".source" { dbg(Lexing.lexeme lexbuf); DSOURCE }
  | ".super" { dbg(Lexing.lexeme lexbuf); DSUPER }
  | ".throws" { dbg(Lexing.lexeme lexbuf); DTHROWS }
  | ".var" { dbg(Lexing.lexeme lexbuf); DVAR }
  | ".debug" { dbg(Lexing.lexeme lexbuf); DDEBUG }
  | ".enclosing" { dbg(Lexing.lexeme lexbuf); DENCLOSING }
  | ".signature" { dbg(Lexing.lexeme lexbuf); DSIGNATURE }
  | ".stack" { dbg(Lexing.lexeme lexbuf); DSTACK }
  | '-'? decimal_numeral '.' decimal_numeral as i { dbg(Lexing.lexeme lexbuf); Num (i) }
  | '-'? decimal_numeral as i                    { dbg(Lexing.lexeme lexbuf); Int (int_of_string i) }
  | '"' ('\\' _ | [ ^ '"'])* '"' as s          { dbg(Lexing.lexeme lexbuf); Str s }
  | ':' { dbg(Lexing.lexeme lexbuf); COLON }
  | '=' { dbg(Lexing.lexeme lexbuf); EQ }
  | [ ^ ' ' '\r' '\n' '\t' '=' ':' ]+ as id { dbg(Lexing.lexeme lexbuf); try
            Hashtbl.find keyword_table id
                                              with Not_found ->
            Word id }
