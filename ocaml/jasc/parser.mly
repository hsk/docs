%{
  open JData
  open JWriter
  open JCode
  open JCodeWriter

(*let debug = debug0*)

  type switchcase = 
    | CaseIntWord of int * string
    | CaseIntInt of int * int

  type switchdefault =
    | DefaultWord of string
    | DefaultInt of int


  let unescape s =
    Scanf.unescaped (String.sub s 1 ((String.length s)- 2))

  let jprim_of_string = function
      | "boolean" -> `Bool
      | "char"    -> `Char
      | "float"   -> `Float
      | "double"  -> `Double
      | "byte"    -> `Byte
      | "short"   -> `Short
      | "int"     -> `Int
      | "long"    -> `Long
      | _         -> assert false

  let replace_dot s =
    let s = String.copy s in
    for i = 0 to String.length s - 1 do
      if s.[i] = '.' then s.[i] <- '/'
    done;
    s

  let split_method name =
    let pos = String.index name '(' in
    (String.sub name 0 pos, String.sub name pos ((String.length name) - pos))

  let split_obj name =
    let name = replace_dot name in
    let pos = String.rindex name '/' in
    (String.sub name 0 pos, String.sub name (pos+1) ((String.length name) - (pos+1)))

  let ctx = ref (new_ctx [||])
  let back_ch = ref !ctx.ch

  let sourcefile = ref None
  let limit_stack = ref 0
  let limit_locals = ref 0
  let pos = ref 0
  let lines = ref []
  let try_catches = ref []
  let throws = ref []
  let label2pos = Hashtbl.create 16
  let pos2label = Hashtbl.create 16

  let init () =
    ctx := new_ctx [||];
    back_ch := !ctx.ch

  let init_method () =
    back_ch := !ctx.ch;
    !ctx.ch <- IO.output_string();
    limit_stack := 255;
    limit_locals := 255;
    pos := 0;
    lines := [];
    try_catches := [];
    throws := [];
    Hashtbl.clear label2pos;
    Hashtbl.clear pos2label

  let label2int l =
    if (Hashtbl.mem label2pos l) then Hashtbl.find label2pos l else
    let pos = -(1 + (Hashtbl.length pos2label)) in
    Hashtbl.add pos2label pos l;
    pos

  let labelint i =
    label2int (string_of_int i)

  let addlabel l i =
    Hashtbl.add label2pos l i

  let add n c =
    let p = !pos in
    pos := !pos + n;
    (p, c)

  let mkcode codes =
    let realloc p l =
      if l >= 0 then l - p else
      let label = Hashtbl.find pos2label l in
      (Hashtbl.find label2pos label) - p
    in
    let code = Array.create !pos OpInvalid in
    List.iter (function 
      | (p, OpInvalid) -> ()
      | (p, OpIfEq(l)) -> code.(p) <- OpIfEq(realloc p l)
      | (p, OpIfNe(l)) -> code.(p) <- OpIfNe(realloc p l)
      | (p, OpIfLt(l)) -> code.(p) <- OpIfLt(realloc p l)
      | (p, OpIfGe(l)) -> code.(p) <- OpIfGe(realloc p l)
      | (p, OpIfGt(l)) -> code.(p) <- OpIfGt(realloc p l)
      | (p, OpIfLe(l)) -> code.(p) <- OpIfLe(realloc p l)
      | (p, OpICmpEq(l)) -> code.(p) <- OpICmpEq(realloc p l)
      | (p, OpICmpNe(l)) -> code.(p) <- OpICmpNe(realloc p l)
      | (p, OpICmpLt(l)) -> code.(p) <- OpICmpLt(realloc p l)
      | (p, OpICmpGe(l)) -> code.(p) <- OpICmpGe(realloc p l)
      | (p, OpICmpGt(l)) -> code.(p) <- OpICmpGt(realloc p l)
      | (p, OpICmpLe(l)) -> code.(p) <- OpICmpLe(realloc p l)
      | (p, OpACmpEq(l)) -> code.(p) <- OpACmpEq(realloc p l)
      | (p, OpACmpNe(l)) -> code.(p) <- OpACmpNe(realloc p l)
      | (p, OpGoto(l)) -> code.(p) <- OpGoto(realloc p l)
      | (p, OpJsr(l)) -> code.(p) <- OpJsr(realloc p l)
      | (p, OpLookupSwitch(d,cases)) ->
        let cases = List.map(fun(a,b)-> (a ,realloc p b)) cases in
        code.(p) <- OpLookupSwitch(realloc p d ,cases)
      | (p, OpTableSwitch (def, low, high, defs)) ->
        let defs = Array.map (realloc p) defs in
        code.(p) <- OpTableSwitch(realloc p def,low, high, defs)
      | (p, c) -> code.(p) <- c
    ) codes;
    let lines = if !lines = [] then None else Some (List.rev !lines) in
    let try_catches = List.map (fun {e_start;e_end;e_handler;e_catch_type} ->
        {
          e_start = realloc 0 e_start;
          e_end = realloc 0 e_end;
          e_handler = realloc 0 e_handler;
          e_catch_type = e_catch_type;
        }
    ) (List.rev !try_catches) in
    let throws = List.rev !throws in
    (code,lines, try_catches, throws)
%}

/* Directives (words beginning with a '.') */
%token DCATCH DCLASS DEND DFIELD DLIMIT DLINE DMETHOD DSET DSUPER
%token DSOURCE DTHROWS DVAR DIMPLEMENTS DINTERFACE DBYTECODE DDEBUG
%token DENCLOSING DSIGNATURE DSTACK DATTRIBUTE DDEPRECATED DINNER
%token DANNOTATION

/* keywords for directives */
%token USING IS FROM METHOD SIGNATURE STACK OFFSET LOCALS FIELD CLASS
%token TO INNER OUTER VISIBLE INVISIBLE VISIBLEPARAM INVISIBLEPARAM USE

/* access types */
%token ABSTRACT FINAL INTERFACE NATIVE PRIVATE PROTECTED PUBLIC STATIC
%token SYNCHRONIZED TRANSIENT VOLATILE
/* added these for java 1.5 compliance : */
%token ANNOTATION ENUM BRIDGE VARARGS STRICT SYNTHETIC

/* complex instructions */
%token LOOKUPSWITCH TABLESWITCH DEFAULT

/* special symbols */
%token EQ SEP COLON

%token <string> Str
%token <string> Word
%token <string * string> Insn
%token <int> Int
%token <string> Num
%token <string> Relative

%token EOF

%start jas_file
%type <jclass> jas_file

%%

/* The grammar */

jas_file :
  | sep jasmin_header inners fields methods
    {debug "jasfile@."; $2 $3 $4 $5 }

sep :
  | SEP { init () }
  |     { init () }

jasmin_header :
  | bytecode_spec /* 1 */
      source_spec/* 2 */
      class_spec /* 3 */
      super_spec /* 4 */
      implements /* 5 */
      signature_spec /* 6 */
      enclosing_spec /* 7 */
      deprecated_spec /* 8 */
      annotations /* 9 */
      generic_attributes /* 10 */
      debug_extension /* 11 */
    {

      let (class_or_interface, access, name) = $3 in
      let attrs = $10 in
      let attrs = match $2 with
      | None -> attrs
      | Some filename -> AttrSourceFile filename :: attrs
      in
      match class_or_interface with
      | "class" ->
        (fun inners fields methods ->
          ignore (const !ctx (ConstClass name));
          {
            cversion = $1;
            consts = ctx_to_consts !ctx;
            cpath = name;
            csuper = $4;
            caccs = JSuper :: access;
            cinterfaces = $5;
            cfields = fields;
            cmethods = methods;
            cattrs = (* $2::$7::$8::$9::$11 @ *) attrs;
            cinner_types = inners;
            ctypes = $6;
          }
        )
      | "interface" ->
        (fun inners fields methods ->
          ignore (const !ctx (ConstClass name));
          {
            cversion = $1;
            consts = ctx_to_consts !ctx;
            cpath = name;
            csuper = $4;
            caccs = JInterface :: access;
            cinterfaces = $5;
            cfields = fields;
            cmethods = methods;
            cattrs = (* $2::$7::$8::$9::$11 @ *) attrs;
            cinner_types = inners;
            ctypes = $6;
          }

          (*
          let fields = List.fold_left (fun fields (fs,f) ->
            let f = {
                if_signature = f.cf_signature ;
                if_class_signature = f.cf_class_signature;
                if_generic_signature = f.cf_generic_signature;
                if_synthetic = f.cf_synthetic;
                if_value = f.cf_value;
                if_annotations = f.cf_annotations;
                if_other_flags = f.cf_other_flags;
                if_attrs = f.cf_attrs;
              }
            in
            JBasics.FieldMap.add fs f fields
          ) JBasics.FieldMap.empty fields in
          let methods = List.fold_left (fun methods -> function
            (ms, ConcreteMethod {
              cm_signature = signature;
              cm_class_method_signature = class_method_signature;
              cm_access = access;
              cm_generic_signature = generic_signature;
              cm_bridge = bridge;
              cm_varargs = varargs;
              cm_synthetic = synthetic;
              cm_other_flags = other_flags;
              cm_exceptions = exceptions;
              cm_attrs = attrs;
              cm_annotations = annotations;
            }) ->
            let m = {
              am_signature = signature;
              am_class_method_signature = class_method_signature;
              am_access =
                begin match access with
                | `Default -> `Default
                | `Protected -> `Protected
                | `Public -> `Public
                | `Private -> assert false
                end
              ;
              am_generic_signature = generic_signature;
              am_bridge = bridge;
              am_varargs = varargs;
              am_synthetic = synthetic;
              am_other_flags = other_flags;
              am_exceptions = exceptions;
              am_attrs = attrs;
              am_annotations = annotations;
              am_annotation_default = None;
            } in
            JBasics.MethodMap.add ms m methods
          ) JBasics.MethodMap.empty methods in
          JInterface {
            i_name = name;
            i_version = $1;
            i_access = if List.mem `Public access then `Public else `Default;
            i_interfaces = $5;
            i_generic_signature = $6;
            i_consts = [||];
            i_sourcefile = $2;
            i_deprecated = $8;
            i_source_debug_extention = $11;
            i_inner_classes = inners;
            i_initializer = None;
            i_annotation = false;
            i_annotations = $9;
            i_other_attrs = $10;
            i_other_flags = [];
            i_fields = fields;
            i_methods = methods;
          }
          *)
        )
    }

/* ---- Signature specification */
signature_spec :
  | DSIGNATURE signature_expr SEP { [] }
  | /* empty */ { [] }

signature_expr :
  | Str {  $1 }

/* ---- Deprecated attribute */
deprecated_spec :
  | DDEPRECATED deprecated_expr SEP { true }
  | /* nothing */ { false }

deprecated_expr :
  | { () }

/* ---- Bytecode version specification */
bytecode_spec :
  | DBYTECODE Num SEP
    {
      let (major, minor) =
        begin try
          let pos = String.index $2 '.' in
          let len = String.length $2 in
          (String.sub $2 0 pos, String.sub $2 (pos+1) (len-pos-1))
        with e -> ($2,"0")
        end
      in
      (int_of_string major, int_of_string minor)
    }
  | /* nothing */
    {
      (45, 3)
    }

/* ---- Source specification */
source_spec :
  | DSOURCE Str SEP { Some $2 }
  | DSOURCE Word SEP { Some $2 }
  | /* nothing */ { !sourcefile }

/* ---- Class specification */
class_spec :
  | DCLASS access classname SEP
    {
      ("class",$2,$3)
    }
  | DINTERFACE access classname SEP
    {
      ("interface",$2,$3)
    }

classname :
  | Word { JReader.expand_path (replace_dot $1) }

access :
  | access_list { $1 }

  access_list :
    | access_items { $1 }
    | { [] }

    access_items :
      | access_item access_items { $1::$2 }
      | access_item { [$1] }

      access_item :
        | PUBLIC { JPublic }
        | PRIVATE { JPrivate }
        | PROTECTED { JProtected }
        | STATIC { JStatic }
        | FINAL { JFinal }
        | SYNCHRONIZED { JSynchronized }
        | VOLATILE { JVolatile }
        | TRANSIENT { JTransient }
        | NATIVE { JNative }
        | INTERFACE { JInterface }
        | ABSTRACT { JAbstract }
        | ANNOTATION { JAnnotation }
        | ENUM { JEnum }
        | BRIDGE { JBridge }
        | VARARGS { JVarArgs }
        | STRICT { JStrict }
        | SYNTHETIC { JSynthetic }

/* --- Superclass specification */
super_spec :
  | DSUPER classname SEP { TObject($2,[]) }

/* ---- Implements specification */
implements :
  | implements_list { $1 }
  | /* empty */ { [] }

  implements_list :
    | implements_spec implements_list { $1::$2 }
    | implements_spec { [$1] }

    implements_spec :
      | DIMPLEMENTS classname SEP { TObject($2,[]) }

/* ---- Annotation specification */
/* TODO */
annotations : 
  | ann_cls_list { [] }
  | /* empty */ { [] }

  ann_cls_list :
    | ann_cls_list ann_cls_spec
      { $2::$1 }
    | ann_cls_spec
      { [$1] }

    ann_cls_spec :
      | ann_cls_expr ann_arglist endannotationsep
        { "" }

      ann_cls_expr :
        | DANNOTATION ann_clf_expr
          { () }

endannotationsep :
  | endannotation SEP { () }

endannotation :
  | DEND ANNOTATION { () }

ann_clf_expr :
  | VISIBLE classname SEP
    { () }
  | INVISIBLE classname SEP
    { () }

ann_met_expr :
  | VISIBLE classname SEP
    { () }
  | INVISIBLE classname SEP
    { () }
  | VISIBLEPARAM Int classname SEP
    { () }
  | INVISIBLEPARAM Int classname SEP
    { () }

ann_arglist :
  | ann_arg_list { [] }
  | /* empty */ { [] }

  ann_arg_list :
    | ann_arg_list ann_arg_spec
      { $2::$1 }
    | ann_arg_spec
      { [$1] }

    ann_arg_spec :
      | ann_arg_expr EQ ann_value_list
        { ($1,$3) }

      ann_arg_expr :
        | Word Word
          { "" }
        | Word Word Word
          { "" }

ann_def_spec :
  | DEFAULT SEP
    { () }

ann_value_list :
  | ann_value_items SEP
    { $1 }
  | ann_ann_list
    { $1 }

ann_value_items :
  | ann_value_items ann_value
    { $2::$1 }
  | ann_value
    { [$1] }

  ann_value :
    | any_item
      { "" }

ann_ann_list :
  | ann_ann_list ann_ann_value { $2::$1 }
  | ann_ann_value { [$1] }

  ann_ann_value :
    | DANNOTATION ann_nest ann_arglist endannotationsep
      { "" }

    ann_nest :
      | SEP { () }

ann_def_val :
  | ann_def_expr EQ ann_value_list
    { () }

  ann_def_expr :
    | Word
      { () }
    | Word Word
      { () }

/* ---- SourceDebugExtension attribute specification */
debug_extension :
  | debug_list { Some (String.concat " " (List.rev $1)) }
  | /* empty */ { None }

  debug_list :
    | debug_list debug_spec
      { $2::$1 }
    | debug_spec
      { [$1] }

    debug_spec :
      | DDEBUG Str SEP { $2 }

/* ---- EnclosingMethod attribute specification */
enclosing_spec :
  | DENCLOSING METHOD Word SEP
    { (*
      let (name, ty) = split_method $3 in
      let (vts, ovt) = JParseSignature.parse_method_descriptor ty in
      let ms = JBasics.make_ms name vts ovt in
      Some (!cn,Some (ms))
      *)
      None
    }
  | /* nothing */ { None }

/* ---- Generic attributes specification */
generic_attributes :
  | generic_list { [] (*$1*) }
  | /* empty */ { [] }

  generic_list :
    | generic_list generic_spec { $2::$1 }
    | generic_spec { [$1] }

    generic_spec :
      | DATTRIBUTE generic_expr SEP { $2 }

generic_expr :
  | Word Str { ($1,$2) } /* TODO check str escape */

/* ---- Fields */
fields :
  | field_list { $1 }
  | { [] }

  field_list :
    | field_spec field_list { $1::$2 }
    | field_spec { [$1] }

    field_spec :
      | DFIELD access Word Word SIGNATURE Str optional_default SEP
        {
          let ty = JReader.parse_ty $4 in
          { jf_name = $3;
            jf_kind = JKField;
            jf_vmty = ty; jf_ty = ty;
            jf_throws = []; jf_types = []; jf_accs = $2;
            jf_attrs = []; jf_const = None; jf_code = None
          }

        }
      | DFIELD access Word Word optional_default SEP
        {
          let ty = JReader.parse_ty $4 in
          { jf_name = $3; jf_kind = JKField;
            jf_vmty = ty; jf_ty = ty;
            jf_throws = []; jf_types = []; jf_accs = $2;
            jf_attrs = []; jf_const = None; jf_code = None
          }
        }
      | DFIELD field_start field_exts endfield
        {
          let (access, name, ty, opt) = $2 in
          let ty = JReader.parse_ty ty in
          { jf_name = name; jf_kind = JKField;
            jf_vmty = ty; jf_ty = ty;
            jf_throws = []; jf_types = []; jf_accs = access;
            jf_attrs = []; jf_const = None; jf_code = None
          }
        }

      /* default value for a field */
      optional_default :
        | EQ item { Some $2 }
        | /* empty */ { None }

      /* multiline form of field description */
      field_start :
        | access Word Word optional_default SEP
          { ($1,$2,$3,$4) }

      endfield :
        | DEND FIELD SEP
          { () }

      field_exts :
        | field_ext_list { $1 }
        | /* empty */ { [] }

        field_ext_list :
          | field_ext_list field_ext_expr { $2::$1 }
          | field_ext_expr { [$1] }

          field_ext_expr :
            | DSIGNATURE signature_expr SEP
              { "" }
            | DATTRIBUTE generic_expr SEP
              { "" }
            | DDEPRECATED deprecated_expr SEP
              { "" }
            | DANNOTATION ann_clf_expr ann_arglist endannotationsep
              { "" }

/* an item is an integer, a float/double/long, or a quoted string  */
item :
  | Int
    { () }
  | Num
    { () }
  | Str
    { () }

/* an item is any possible type */
any_item :
  | Word
    { () }
  | item
    { () }

/* ---- Inner classes */
inners :
  | inner_list { debug "inners@."; [] (* TODO List.rev $1*) }
  | /* empty */ { [] }

  inner_list :
    | inner_list inner_spec { $2::$1 }
    | inner_spec { [$1] }

    inner_spec :
      | DINNER CLASS access inner_name inner_inner inner_outer SEP
        {
          ""
          (*
          {
            ic_class_name = $5;
            ic_outer_class_name = $6;
            ic_source_name = $4;
            ic_access = cf_access $3;
            ic_static = List.mem `Static $3;
            ic_final = List.mem `Final $3;
            ic_synthetic = List.mem `Synthetic $3;
            ic_annotation = List.mem `Annotation $3;
            ic_enum = List.mem `Enum $3;
            ic_other_flags = []; *)(* TODO *)(*
            ic_type = `ConcreteClass *)(* TODO `Abstract *)(*
          }
          *)
        }
      | DINNER INTERFACE access inner_name inner_inner inner_outer SEP
        {
          ""
          (*
          {
            ic_class_name = $5;
            ic_outer_class_name = $6;
            ic_source_name = $4;
            ic_access = cf_access $3;
            ic_static = List.mem `Static $3;
            ic_final = List.mem `Final $3;
            ic_synthetic = List.mem `Synthetic $3;
            ic_annotation = List.mem `Annotation $3;
            ic_enum = List.mem `Enum $3;
            ic_other_flags = []; *)(* TODO *)(*
            ic_type = `Interface
          }
          *)
        }

      inner_name :
        | Word { Some $1 }
        | /* empty */ { None }

      inner_inner :
        | INNER classname { Some $2 }
        | /* empty */ { None }

      inner_outer :
        | OUTER classname { Some $2 }
        | /* empty */ { None }

/* ---- Methods */
methods :
  | method_list { List.rev $1 }
  | /* empty */ { [] }

  method_list :
    | method_spec { [$1] }
    | method_list method_spec { $2::$1 }

    method_spec :
      | defmethod statements endmethod
        {
          let(access,(name,ty)) = $1 in
          let code,lines,try_catches,throws = mkcode $2 in
          let attrs = [] in
          let attrs = match lines with
            | None -> attrs
            | Some lines -> AttrLineNumberTable lines::attrs
          in
          let jmethod = {
            max_stack = !limit_stack;
            max_locals = !limit_locals;
            code = code;
            try_catches = try_catches;
            (*c_line_number_table = lines; TODO *)
            (*c_local_variable_table = None;  TODO *)
            (*c_local_variable_type_table = None;  TODO *)
            (*c_stack_map_midp = None;  TODO *)
            (*c_stack_map_java6 = None;  TODO *)
            attrs = attrs; (* TODO *)
          }
          in
          JCodeWriter.encode_code !ctx jmethod;
          let code = IO.close_out !ctx.ch in
          !ctx.ch <- !back_ch;
          (*
          let m = ConcreteMethod {
            cm_signature = ms;
            cm_class_method_signature = JBasics.make_cms !cn ms;
            cm_static = List.mem `Static access;
            cm_final = List.mem `Final access;
            cm_synchronized = List.mem `Synchronized access;
            cm_strict = List.mem `Strict access;
            cm_access = cf_access access;
            cm_generic_signature = None; *)(* TODO *)(*
            cm_bridge = List.mem `Bridge access;
            cm_varargs = List.mem `Varargs access;
            cm_synthetic = List.mem `Synthetic access;
            cm_other_flags = []; *)(* TODO *)(*
            cm_exceptions = throws; *)(* TODO *)(*
            cm_attrs = { synthetic = false; deprecated = false; other = [] };  *)(* TODO *)(*
            cm_annotations = { ma_global = []; ma_parameters = [] }; *)(* TODO *)(*
            cm_implementation = Java (lazy jmethod)
          } in (ms,m)
          *)
          { jf_name = name;
            jf_kind = JKMethod;
            jf_vmty = ty;
            jf_ty = ty;
            jf_throws = List.map (fun jpath -> TObject(jpath,[])) throws;
            jf_types = [];
            jf_accs = access;
            jf_attrs = [AttrUnknown ("Code", code)];
            jf_const = None;
            jf_code = None
          }
        }
      | defmethod endmethod
        {
          let(access,(name,ty)) = $1 in
          !ctx.ch <- !back_ch;
          { jf_name = name;
            jf_kind = JKMethod;
            jf_vmty = ty;
            jf_ty = ty;
            jf_throws = [];
            jf_types = [];
            jf_accs = access;
            jf_attrs = [];
            jf_const = None;
            jf_code = None
          }

        }

      defmethod :
        | DMETHOD access Word SEP
          {
            init_method();
            let (name, ty) = split_method $3 in
            let ty = TMethod(JReader.parse_mty ty) in
            ($2, (name, ty))
          }

      endmethod :
        | DEND METHOD SEP { () }

      /* ---- Statements in a method */
      statements :
        | statements statement { $2::$1 }
        | statement { [$1] }

        statement :
          | stmnt SEP { $1 }

          stmnt :
            | instruction { $1 }
            | directive { $1 }
            | error { add 0 OpInvalid }
            | label { $1 }
            | /* empty */ { add 0 OpInvalid }

            /* label: */
            label :
              | Word COLON {
                addlabel $1 !pos;
                add 0 OpInvalid
               }
              | Int COLON instruction {
                let (pos, _) = $3 in
                addlabel (string_of_int $1) pos;
                $3
              }

            /* Directives (.catch, .set, .limit, etc.) */
            directive :
              | DVAR var_expr
                {
                  failwith "TODO: .var"
                }
              | DLIMIT limit_expr { add 0 OpInvalid }
              | DLINE line_expr { add 0 OpInvalid }
              | DTHROWS throws_expr { add 0 OpInvalid }
              | DCATCH catch_expr { add 0 OpInvalid }
              | DSET set_expr
                {
                  failwith "TODO: .set"
                }
              | DSIGNATURE signature_expr
                {
                  failwith "TODO: .signature"
                }
              | DATTRIBUTE generic_expr
                {
                  failwith "TODO: .attribute"
                }
              | DDEPRECATED deprecated_expr
                {
                  failwith "TODO: .deprected"
                }
              | DANNOTATION ann_met_expr ann_arglist endannotation
                {
                  failwith "TODO: .annotation"
                }
              | DANNOTATION ann_def_spec ann_def_val endannotation
                {
                  failwith "TODO: .annotation"
                }
              | DSTACK stackmap
                {
                  failwith "TODO: .stack"
                }

              /* */
              /* .var <num> is <name> <desc> from StartLab to EndLab */
              /* .var <num> is <name> <desc> signature <sign> from StartLab to EndLab */
              /* */
              var_expr : /* TODO */
                | Int IS Word Word optional_signature FROM Word TO Word
                  { ()  (* TODO *)}
                | Int IS Word Word optional_signature
                  { ()  (* TODO *)}
                | Int IS Word Word optional_signature FROM Int TO Int
                  { ()  (* TODO *)}

                /* optional signature specification for a .var */
                optional_signature :
                  | SIGNATURE Str { Some $2 }
                  | /* empty */ { None }


              /* .limit stack <val> */
              /* .limit locals <val> */
              limit_expr :
                | LOCALS Int { limit_locals := $2 } /* .limit locals */
                | STACK Int { limit_stack := $2 } /* .limit stack */
                | Word Int { () }

              /* .line <num> */
              line_expr :
                | Int { lines := (!pos, $1) :: !lines }

              /* .throws <class> */
              throws_expr :
                | classname { throws := $1 :: !throws }

              /* .catch <class> from <label1> to <label2> using <branchlab> */
              catch_expr :
                | classname FROM Word TO Word USING Word
                  {
                    try_catches := {
                      e_start   = label2int $3;
                      e_end     = label2int $5;
                      e_handler = label2int $7;
                      e_catch_type = Some $1
                    } :: !try_catches
                  }
                | classname FROM Int TO Int USING Int
                  {
                    try_catches := {
                      e_start   = labelint $3;
                      e_end     = labelint $5;
                      e_handler = labelint $7;
                      e_catch_type = Some $1
                    } :: !try_catches
                  }

              /* .set <var> = <val> */
              set_expr :
                | Word any_item { () }

              /*        */
              /* .stack */
              /*        */
              /* TODO */
              stackmap :
                | defstack stack_map_frame_desc endstack
                  { () }
                | USE defstack_same stack_map_frame_desc endstack
                  { () }

                defstack_same :
                  | defstack_same_expr LOCALS SEP
                    { () }

                defstack_same_expr :
                  | Int
                    { () }
                  | /* empty */
                    { () }

                defstack :
                  | SEP { () }

                stack_map_frame_desc :
                  | stack_offset_def stack_items
                    { () }

                stack_offset_def :
                  | OFFSET Int SEP
                    { () }
                  | OFFSET Word SEP
                    { () }
                  | /* nothing */
                    { () }

                stack_items :
                  | stack_items stack_item { $2::$1 }
                  | /* nothing */ { [] }

                stack_item :
                  | stack_item_expr SEP { $1 }

                stack_item_expr :
                  | LOCALS Word
                    { "" }
                  | LOCALS Word Word
                    { "" }
                  | LOCALS Word Int
                    { "" }
                  | STACK Word
                    { "" }
                  | STACK Word Word
                    { "" }
                  | STACK Word Int
                    { "" }

                endstack :
                  | DEND STACK { () }

            instruction :
              | simple_instruction { $1 }
              | complex_instruction { $1 }

              /* Various patterns of instruction: */
              /*      instruction [<pattern>] */
              simple_instruction :
                | Insn {
                  match (fst $1) with
                  (* A *)
                  | "aaload" -> add 1 (OpAALoad)
                  | "aastore" -> add 1 (OpAAStore)
                  | "aconst_null" -> add 1 (OpAConstNull)
                  | "aload_0" -> add 1 (OpALoad1 0)
                  | "aload_1" -> add 1 (OpALoad1 1)
                  | "aload_2" -> add 1 (OpALoad1 2)
                  | "aload_3" -> add 1 (OpALoad1 3)
                  | "areturn" -> add 1 (OpAReturn)
                  | "arraylength" -> add 1 (OpArrayLength)
                  | "astore_0" -> add 1 (OpAStore1 0)
                  | "astore_1" -> add 1 (OpAStore1 1)
                  | "astore_2" -> add 1 (OpAStore1 2)
                  | "astore_3" -> add 1 (OpAStore1 3)
                  | "athrow" -> add 1 (OpThrow)
                  (* B *)
                  | "baload" ->add 1 (OpBALoad)
                  | "bastore" -> add 1 (OpBAStore)
                  | "breakpoint" -> add 1 (OpBreakpoint)
                  (* C *)
                  | "caload" ->add 1 (OpCALoad)
                  | "castore" -> add 1 (OpCAStore)
                  (* D *)
                  | "d2f" -> add 1 (OpD2F)
                  | "d2i" -> add 1 (OpD2I)
                  | "d2l" -> add 1 (OpD2L)
                  | "dadd" -> add 1 (OpAdd `Double)
                  | "daload" ->add 1 (OpArrayLoad `Double)
                  | "dastore" -> add 1 (OpArrayStore `Double)
                  | "dcmpg" -> add 1 (OpDCmpG)
                  | "dcmpl" -> add 1 (OpDCmpL)
                  | "dconst_0" -> add 1 (OpDConst( (0.)))
                  | "dconst_1" -> add 1 (OpDConst( (1.)))
                  | "ddiv" -> add 1 (OpDiv `Double)
                  | "dload_0" -> add 1 (OpLoad1 (`Double, 0))
                  | "dload_1" -> add 1 (OpLoad1 (`Double, 1))
                  | "dload_2" -> add 1 (OpLoad1 (`Double, 2))
                  | "dload_3" -> add 1 (OpLoad1 (`Double, 3))
                  | "dmul" -> add 1 (OpMul `Double)
                  | "dneg" -> add 1 (OpNeg `Double)
                  | "drem" -> add 1 (OpRem `Double)
                  | "dreturn" -> add 1 (OpReturn `Double)
                  | "dstore_0" -> add 1 (OpStore1 (`Double, 0))
                  | "dstore_1" -> add 1 (OpStore1 (`Double, 1))
                  | "dstore_2" -> add 1 (OpStore1 (`Double, 2))
                  | "dstore_3" -> add 1 (OpStore1 (`Double, 3))
                  | "dsub" -> add 1 (OpSub `Double)
                  | "dup" -> add 1 (OpDup)
                  | "dup2" -> add 1 (OpDup2)
                  | "dup2_x1" -> add 1 (OpDup2X1)
                  | "dup2_x2" -> add 1 (OpDup2X2)
                  | "dup_x1" -> add 1 (OpDupX1)
                  | "dup_x2" -> add 1 (OpDupX2)
                  (* F *)
                  | "f2i" -> add 1 (OpF2I)
                  | "f2l" -> add 1 (OpF2L)
                  | "f2d" -> add 1 (OpF2D)
                  | "fadd" -> add 1 (OpAdd `Float)
                  | "faload" ->add 1 (OpArrayLoad `Float)
                  | "fastore" -> add 1 (OpArrayStore `Float)
                  | "fcmpg" -> add 1 (OpFCmpG)
                  | "fcmpl" -> add 1 (OpFCmpL)
                  | "fconst_0" -> add 1(OpFConst( (0.)))
                  | "fconst_1" -> add 1(OpFConst( (1.)))
                  | "fconst_2" -> add 1(OpFConst( (2.)))
                  | "fdiv" -> add 1 (OpDiv `Float)
                  | "fload_0" -> add 1 (OpLoad1 (`Float, 0))
                  | "fload_1" -> add 1 (OpLoad1 (`Float, 1))
                  | "fload_2" -> add 1 (OpLoad1 (`Float, 2))
                  | "fload_3" -> add 1 (OpLoad1 (`Float, 3))
                  | "fmul" -> add 1 (OpMul `Float)
                  | "fneg" -> add 1 (OpNeg `Float)
                  | "frem" -> add 1 (OpRem `Float)
                  | "freturn" -> add 1 (OpReturn `Float)
                  | "fstore_0" -> add 1 (OpStore1 (`Float, 0))
                  | "fstore_1" -> add 1 (OpStore1 (`Float, 1))
                  | "fstore_2" -> add 1 (OpStore1 (`Float, 2))
                  | "fstore_3" -> add 1 (OpStore1 (`Float, 3))
                  | "fsub" -> add 1 (OpSub `Float)
                  (* I *)
                  | "i2f" -> add 1 (OpI2F)
                  | "i2d" -> add 1 (OpI2D)
                  | "i2l" -> add 1 (OpI2L)
                  | "iadd" -> add 1 (OpAdd `Int)
                  | "iaload" -> add 1 (OpArrayLoad `Int)
                  | "iand" -> add 1 (OpIAnd)
                  | "iastore" -> add 1 (OpArrayStore `Int)
                  | "iconst_0" -> add 1 (OpIConst( (0l)))
                  | "iconst_1" -> add 1 (OpIConst( (1l)))
                  | "iconst_2" -> add 1 (OpIConst( (2l)))
                  | "iconst_3" -> add 1 (OpIConst( (3l)))
                  | "iconst_4" -> add 1 (OpIConst( (4l)))
                  | "iconst_5" -> add 1 (OpIConst( (5l)))
                  | "iconst_m1" -> add 1 (OpIConst( (-1l)))
                  | "idiv" -> add 1 (OpDiv `Int)
                  | "iload_0" -> add 1 (OpLoad1 (`Int, 0))
                  | "iload_1" -> add 1 (OpLoad1 (`Int, 1))
                  | "iload_2" -> add 1 (OpLoad1 (`Int, 2))
                  | "iload_3" -> add 1 (OpLoad1 (`Int, 3))
                  | "imul" -> add 1 (OpMul `Int)
                  | "ineg" -> add 1 (OpNeg `Int)
                  | "int2byte" -> add 1 (OpI2B)
                  | "int2char" -> add 1 (OpI2C)
                  | "int2short" -> add 1 (OpI2S)
                  (*
                  | "invokedynamic", "method" -> add 1 ()
                  *)
                  | "ior" -> add 1 (OpIOr)
                  | "irem" -> add 1 (OpRem `Int)
                  | "ireturn" -> add 1 (OpReturn `Int)
                  | "ishl" -> add 1 (OpIShl)
                  | "ishr" -> add 1 (OpIShr)
                  | "istore_0" -> add 1 (OpStore1 (`Int, 0))
                  | "istore_1" -> add 1 (OpStore1 (`Int, 1))
                  | "istore_2" -> add 1 (OpStore1 (`Int, 2))
                  | "istore_3" -> add 1 (OpStore1 (`Int, 3))
                  | "isub" -> add 1 (OpSub `Int)
                  | "iushr" -> add 1 (OpIUShr)
                  | "ixor" -> add 1 (OpIXor)
                  (* L *)
                  | "l2f" -> add 1 (OpL2F)
                  | "l2d" -> add 1 (OpL2D)
                  | "l2i" -> add 1 (OpL2I)
                  | "ladd" -> add 1 (OpAdd `Long)
                  | "laload" ->add 1 (OpArrayLoad `Long)
                  | "land" -> add 1 (OpLAnd)
                  | "lastore" -> add 1 (OpArrayStore `Long)
                  | "lcmp" -> add 1 (OpLCmp)
                  | "lconst_0" -> add 1 (OpLConst( (Int64.of_int 0)))
                  | "lconst_1" -> add 1 (OpLConst( (Int64.of_int 1)))
                  | "ldiv" -> add 1 (OpDiv `Long)
                  | "lload_0" -> add 1 (OpLoad1 (`Long, 0))
                  | "lload_1" -> add 1 (OpLoad1 (`Long, 1))
                  | "lload_2" -> add 1 (OpLoad1 (`Long, 2))
                  | "lload_3" -> add 1 (OpLoad1 (`Long, 3))
                  | "lmul" -> add 1 (OpMul `Long)
                  | "lneg" -> add 1 (OpNeg `Long)
                  | "lor" -> add 1 (OpLOr)
                  | "lrem" -> add 1 (OpRem `Long)
                  | "lreturn" -> add 1 (OpReturn `Long)
                  | "lshl" -> add 1 (OpLShl)
                  | "lshr" -> add 1 (OpLShr)
                  | "lstore_0" -> add 1 (OpStore1 (`Long, 0))
                  | "lstore_1" -> add 1 (OpStore1 (`Long, 1))
                  | "lstore_2" -> add 1 (OpStore1 (`Long, 2))
                  | "lstore_3" -> add 1 (OpStore1 (`Long, 3))
                  | "lsub" -> add 1 (OpSub `Long)
                  | "lushr" -> add 1 (OpLUShr)
                  | "lxor" -> add 1 (OpLXor)
                  (* M *)
                  | "monitorenter" -> add 1 (OpMonitorEnter)
                  | "monitorexit" -> add 1 (OpMonitorExit)
                  (* N *)
                  | "nop" -> add 1 (OpNop)
                  (* P *)
                  | "pop" -> add 1 (OpPop)
                  | "pop2" -> add 1 (OpPop2)
                  (* R *)
                  | "return" -> add 1 (OpReturnVoid)
                  (* S *)
                  | "saload" -> add 1 (OpSALoad)
                  | "sastore" -> add 1 (OpSAStore)
                  | "swap" -> add 1 (OpSwap)
                  | a -> Printf.printf "Inst(%S, %S)@." a (snd $1); assert false
                }
                | Insn Int Int {
                  match(fst $1,snd $1, $2,$3)with
                  | "iinc", "ii", i1, i2 -> add 3 (OpIInc (i1, i2, false))
                  | "iinc", "Ii", i1, i2 -> add 6 (OpIInc (i1, i2, true))
                  | a,b,i1,i2 ->
                    Printf.printf "InstIntInt(%S, %S, %d, %d)@." a b i1 i2;
                    assert false
                }
                | Insn Int {
                  match(fst $1,snd $1, $2)with
                  (* A *)
                  | "aload", "i", n -> add 2 (OpALoad (n, false))
                  | "aload", "I", n -> add 3 (OpALoad (n, true))
                  | "astore", "i", n -> add 2 (OpAStore (n, false))
                  | "astore", "I", n -> add 3 (OpAStore (n, true))
                  (* B *)
                  | "bipush", "i", n -> add 2 (OpBIPush(n))
                  (* D *)
                  | "dload", "i", i -> add 2 (OpLoad (`Double, i, false))
                  | "dload", "I", n -> add 3 (OpLoad (`Double, n, true))
                  | "dstore", "i", i -> add 2 (OpStore (`Double, i, false))
                  | "dstore", "I", n -> add 3 (OpStore (`Double, n, true))
                  (* F *)
                  | "fload", "i", i -> add 2 (OpLoad (`Float, i, false))
                  | "fload", "I", n -> add 3 (OpLoad (`Float, n, true))
                  | "fstore", "i", i -> add 2 (OpStore (`Float, i, false))
                  | "fstore", "I", n -> add 3 (OpStore (`Float, n, true))
                  (* G *)
                  | "goto", "label", n -> add 3 (OpGoto (labelint n))
                  | "goto_w", "label", n -> add 3 (OpGoto (labelint n))
                  (* I *)
                  | "if_acmpeq", "label", n -> add 3 (OpACmpEq (labelint n))
                  | "if_acmpne", "label", n -> add 3 (OpACmpNe (labelint n))
                  | "if_icmpeq", "label", n -> add 3 (OpICmpEq (labelint n))
                  | "if_icmpge", "label", n -> add 3 (OpICmpGe (labelint n))
                  | "if_icmpgt", "label", n -> add 3 (OpICmpGt (labelint n))
                  | "if_icmple", "label", n -> add 3 (OpICmpLe (labelint n))
                  | "if_icmplt", "label", n -> add 3 (OpICmpLt (labelint n))
                  | "if_icmpne", "label", n -> add 3 (OpICmpNe (labelint n))
                  | "ifeq", "label", n -> add 3 (OpIfEq (labelint n))
                  | "ifge", "label", n -> add 3 (OpIfGe (labelint n))
                  | "ifgt", "label", n -> add 3 (OpIfGt (labelint n))
                  | "ifle", "label", n -> add 3 (OpIfLe (labelint n))
                  | "iflt", "label", n -> add 3 (OpIfLt (labelint n))
                  | "ifne", "label", n -> add 3 (OpIfNe (labelint n))
                  | "ifnonnull", "label", n -> add 3 (OpIfNonNull (labelint n))
                  | "ifnull", "label", n -> add 3 (OpIfNull (labelint n))
                  | "iload", "i", n -> add 2 (OpLoad (`Int, n, false))
                  | "iload", "I", n -> add 3 (OpLoad (`Int, n, true))
                  | "istore", "i", i -> add 2 (OpStore (`Int, i, false))
                  | "istore", "I", i -> add 3 (OpStore (`Int, i, true))
                  (* I *)
                  | "ldc", "constant", n -> add 2 (OpLdc1(const !ctx (ConstInt (Int32.of_int n))))
                  | "ldc2_w", "bigconstant", d -> add 3 (OpLdc2w(const !ctx (ConstLong (Int64.of_int d))))
                  | "lload", "i", i -> add 2 (OpLoad (`Long, i, false)) 
                  | "lload", "I", n -> add 3 (OpLoad (`Long, n, true))
                  | "lstore", "i", l -> add 2 (OpStore (`Long, l, false))
                  | "lstore", "I", l -> add 3 (OpStore (`Long, l, true))

                  | "jsr","label",label -> add 3 (OpJsr(labelint label))
                  | "jsr_w","label",label -> add 3 (OpJsr(labelint label))
                  (* R *)
                  | "ret", "i", n -> add 2 (OpRet (n, false))
                  | "ret", "I", n -> add 3 (OpRet (n, true))
                  (* S *)
                  | "sipush", "i", n -> add 3 (OpSIPush n)
                  | a,b,i1 ->
                    Printf.printf "InstInt(%S, %S, %d)@." a b i1;
                    assert false
                }
                | Insn Num {
                  match(fst $1,snd $1, $2)with
                  (* I *)
                  | "ldc", "constant", s -> add 2 (OpLdc1(const !ctx (ConstFloat (float_of_string s))))
                  | "ldc2_w", "bigconstant", d -> add 3 (OpLdc2w(const !ctx (ConstDouble (float_of_string d))))
                  | a,b,s ->
                    Printf.printf "InstNum(%S, %S, %S)@." a b s;
                    assert false
                }
                | Insn Word { 
                  match(fst $1,snd $1, $2)with
                  (* A *)
                  | "anewarray", "class", o ->
                  add 3 (OpANewArray (const !ctx (ConstClass (JReader.expand_path o))))
                  (* C *)
                  | "checkcast", "class", w ->
                    add 3 (OpCheckCast (const !ctx (ConstClass (JReader.expand_path w))))
                  (* G *)
                  | "goto", "label", l -> add 3 (OpGoto(label2int l))
                  | "goto_w", "label", l -> add 3 (OpGoto(label2int l))
                  (* I *)
                  | "if_acmpeq", "label", l -> add 3 (OpACmpEq (label2int l))
                  | "if_acmpne", "label", l -> add 3 (OpACmpNe (label2int l))
                  | "if_icmpeq", "label", l -> add 3 (OpICmpEq (label2int l))
                  | "if_icmpge", "label", l -> add 3 (OpICmpGe (label2int l))
                  | "if_icmpgt", "label", l -> add 3 (OpICmpGt (label2int l))
                  | "if_icmple", "label", l -> add 3 (OpICmpLe (label2int l))
                  | "if_icmplt", "label", l -> add 3 (OpICmpLt (label2int l))
                  | "if_icmpne", "label", l -> add 3 (OpICmpNe (label2int l))
                  | "ifeq", "label", l -> add 3 (OpIfEq (label2int l))
                  | "ifge", "label", l -> add 3 (OpIfGe (label2int l))
                  | "ifgt", "label", l -> add 3 (OpIfGt (label2int l))
                  | "ifle", "label", l -> add 3 (OpIfLe (label2int l))
                  | "iflt", "label", l -> add 3 (OpIfLt (label2int l))
                  | "ifne", "label", l -> add 3 (OpIfNe (label2int l))
                  | "instanceof", "class", o -> add 3 (OpInstanceOf (const !ctx (ConstClass (JReader.expand_path o))))
                  | "invokenonvirtual", "method", m ->
                    let (obj,mty) = split_method m in
                    let (name,o) = split_obj obj in
                    let jpath = JReader.expand_path name in
                    let mty = JReader.parse_mty mty in
                    add 3 (OpInvokeSpecial (const !ctx (ConstMethod (jpath, o, mty))))
                  | "invokestatic", "method", m ->
                    let (obj,mty) = split_method m in
                    let (name,o) = split_obj obj in
                    let jpath = JReader.expand_path name in
                    let mty = JReader.parse_mty mty in
                    add 3 (OpInvokeStatic (const !ctx (ConstMethod (jpath, o, mty))))
                  | "invokevirtual", "method", m ->
                    let (obj,mty) = split_method m in
                    let (name,o) = split_obj obj in
                    let jpath = JReader.expand_path name in
                    let mty = JReader.parse_mty mty in
                    add 3 (OpInvokeVirtual (const !ctx (ConstMethod (jpath, o, mty))))
                  (* J *)
                  | "jsr","label",label -> add 3 (OpJsr(label2int label))
                  | "jsr_w","label",label -> add 3 (OpJsr(label2int label))
                  | "ldc", "constant", d -> add 2 (OpLdc1(const !ctx (ConstFloat (float_of_string d))))
                  | "ldc2_w", "bigconstant", d -> add 3 (OpLdc2w(const !ctx (ConstDouble (float_of_string d))))
                  (* N *)
                  | "new", "class", o -> add 3 (OpNew (const !ctx (ConstClass (JReader.expand_path (replace_dot o)))))
                  | "newarray", "atype", t -> add 2 (OpNewArray (jprim_of_string t))
                  | a,b,s ->
                    Printf.printf "InstWord(%S, %S, %S)@." a b s;
                    assert false
                }
                | Insn Word Int {
                  match(fst $1,snd $1, $2,$3)with
                  (* I *)
                  | "invokeinterface", "interface", m, i2 ->
                    let (obj,mty) = split_method m in
                    let (name,o) = split_obj obj in
                    let jpath = JReader.expand_path name in
                    let mty = JReader.parse_mty mty in
                    add 5(OpInvokeInterface (const !ctx (ConstInterfaceMethod (jpath, o, mty)), i2))
                  (* M *)
                  | "multianewarray", "marray", t, i ->
                    add 4 (OpAMultiNewArray (const !ctx (ConstClass (JReader.expand_path t)), i))
                  | a,b,s,i ->
                    Printf.printf "InstWordInt(%S, %S, %S, %d)@." a b s i;
                    assert false
                }
                | Insn Word Word {
                  match(fst $1, $2,$3)with
                  | "getfield", cf, ty ->
                    let (c,f) = split_obj(cf) in
                    let jpath = JReader.expand_path c in
                    let ty = JReader.parse_ty ty in
                    add 3 (OpGetField (const !ctx (ConstField(jpath,f,ty))))
                  | "getstatic", cf, ty ->
                    let (c,f) = split_obj(cf) in
                    let jpath = JReader.expand_path c in
                    let ty = JReader.parse_ty ty in
                    add 3 (OpGetStatic (const !ctx (ConstField(jpath,f,ty))))
                  | "putfield", cf, ty ->
                    let (c,f) = split_obj(cf) in
                    let jpath = JReader.expand_path c in
                    let ty = JReader.parse_ty ty in
                    add 3 (OpPutField (const !ctx (ConstField(jpath,f,ty))))
                  | "putstatic", cf, ty ->
                    let (c,f) = split_obj(cf) in
                    let jpath = JReader.expand_path c in
                    let ty = JReader.parse_ty ty in
                    add 3 (OpPutStatic (const !ctx (ConstField(jpath,f,ty))))
                  | a, b, s2 ->
                    Printf.printf "InstWordWord(%S, %S, %S, %S)@." a b (snd $1) s2;
                    assert false
                }
                | Insn Str { 
                  match(fst $1,snd $1, unescape $2)with
                  | "ldc", "constant", s -> add 2 (OpLdc1(const !ctx(ConstString s)))
                  | a,b,s ->
                    Printf.printf "InstStr(%S, %S, %S)@." a b s;
                    assert false
                }
                | Insn Relative {
                  match(fst $1, snd $1,$2)with
                  | a,b,s ->
                    Printf.printf "InstRelative(%S, %S, %S)@." a b s;
                    assert false
                }

              /* complex (i.e. multiline) instructions */
              /*      lookupswitch <lookup> */
              /*      tableswitch  <table> */
              complex_instruction :
                | LOOKUPSWITCH lookup { $2 }
                | TABLESWITCH table { $2 }

                /* lookupswitch */
                /*     <value> : <label> */
                /*     <value> : <label> */
                /*     ... */
                /*     default : <label> */
                lookup :
                  | lookup_args lookup_list lookup_default
                    {
                      let a =
                        match $3 with
                        | DefaultInt i -> labelint i
                        | DefaultWord l -> label2int l
                      in
                      let ls = List.map(function
                        | CaseIntInt(i, j)-> (Int32.of_int i, labelint j)
                        | CaseIntWord(i, j) -> (Int32.of_int i, label2int j)
                      ) $2 in
                      let padding_size = (4 - (!pos + 1) mod 4) mod 4 in
                      let n = 9 + padding_size + 8 * (List.length $2) in
                      add n (OpLookupSwitch (a, ls))

                    }

                  lookup_args :
                    | SEP { () } /* no arguments to lookupswitch */

                  lookup_list :
                    | lookup_entry lookup_list { $1 :: $2 }
                    | lookup_entry { [$1] }

                    lookup_entry :
                      | Int COLON Word SEP { CaseIntWord($1,$3) }
                      | Int COLON Int SEP { CaseIntInt($1,$3) }

                  lookup_default :
                    | DEFAULT COLON Word { DefaultWord $3 }
                    | DEFAULT COLON Int { DefaultInt $3 }

                /* tableswitch <low> [<high>] */
                /*     <label> */
                /*     <label> */
                /*     ... */
                /*     default : <label> */
                table :
                  | table_args table_list table_default
                    {
                      (* T *)
                      let (low,high,defs,def) = (fst $1,snd $1,$2,$3) in
                      let default2int = function
                        | DefaultInt i -> labelint i
                        | DefaultWord l -> label2int l
                      in
                      let defs = List.map default2int defs in
                      let defs = Array.of_list defs in
                      let padding_size = (4 - ((!pos + 1) mod 4)) mod 4 in
                      let n = 13 + padding_size + 4 * (Array.length defs) in
                      let high = if high = -1 then Array.length defs - 1 + low else high in
                      add n (OpTableSwitch ((default2int def), Int32.of_int low, Int32.of_int high, defs))
                    }

                  table_args :
                    | Int SEP { ($1,-1) } /* one argument : the <low> parameter */
                    | Int Int SEP { ($1, $2) } /* two arguments : <low> and <high> parameters */

                  table_list :
                    | table_entry table_list { $1::$2 }
                    | table_entry { [$1] }

                    table_entry :
                      | Word SEP { DefaultWord($1) }
                      | Int SEP { DefaultInt($1) }

                  table_default :
                    | DEFAULT COLON Word { DefaultWord($3) }
                    | DEFAULT COLON Int { DefaultInt($3) }

