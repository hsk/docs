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

  let cbasic = function 
      | "boolean" -> "Z"
      | "byte"    -> "B"
      | "char"    -> "C"
      | "short"   -> "S"
      | "int"     -> "I"
      | "long"    -> "J"
      | "float"   -> "F"
      | "double"  -> "D"
      | "void"    -> "V"
      | _ -> assert false

  let java_basic_type_of_string = function
      | "boolean" -> `Bool
      | "char" -> `Char
      | "float" -> `Float
      | "double" -> `Double
      | "byte" -> `Byte
      | "short" -> `Short
      | "int" -> `Int
      | "long" -> `Long
      | _ -> assert false

  let cf_access a =
    if List.mem `Public a then `Public else
    if List.mem `Protected a then `Protected else
    if List.mem `Private a then `Private else
    `Default

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

  let back_ch = ref (IO.output_string())
  let ctx = ref (new_ctx !back_ch ([||]))

  let sourcefile = ref None
  let cn = ref ([],"_")
  let limit_stack = ref 0
  let limit_locals = ref 0
  let pos = ref 0
  let lines = ref []
  let excs = ref []
  let throws = ref []
  let label2pos = Hashtbl.create 16
  let pos2label = Hashtbl.create 16

  let init () =
    back_ch := IO.output_string();
    ctx := new_ctx !back_ch ([||])

  let init_method () =
    back_ch := !ctx.ch;
    !ctx.ch <- IO.output_string();
    limit_stack := 255;
    limit_locals := 255;
    pos := 0;
    lines := [];
    excs := [];
    throws := [];
    Hashtbl.clear label2pos;
    Hashtbl.clear pos2label

  let label2int l =
    if (Hashtbl.mem label2pos l) then Hashtbl.find label2pos l else
    let pos = -(1 + (Hashtbl.length pos2label)) in
    Hashtbl.add pos2label pos l;
    pos

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
    let code = Array.create !pos JCode.OpInvalid in
    List.iter (function 
      | (p, JCode.OpInvalid) -> ()
      | (p, JCode.OpIfEq(l)) -> code.(p) <- JCode.OpIfEq(realloc p l)
      | (p, JCode.OpIfNe(l)) -> code.(p) <- JCode.OpIfNe(realloc p l)
      | (p, JCode.OpIfLt(l)) -> code.(p) <- JCode.OpIfLt(realloc p l)
      | (p, JCode.OpIfGe(l)) -> code.(p) <- JCode.OpIfGe(realloc p l)
      | (p, JCode.OpIfGt(l)) -> code.(p) <- JCode.OpIfGt(realloc p l)
      | (p, JCode.OpIfLe(l)) -> code.(p) <- JCode.OpIfLe(realloc p l)
      | (p, JCode.OpICmpEq(l)) -> code.(p) <- JCode.OpICmpEq(realloc p l)
      | (p, JCode.OpICmpNe(l)) -> code.(p) <- JCode.OpICmpNe(realloc p l)
      | (p, JCode.OpICmpLt(l)) -> code.(p) <- JCode.OpICmpLt(realloc p l)
      | (p, JCode.OpICmpGe(l)) -> code.(p) <- JCode.OpICmpGe(realloc p l)
      | (p, JCode.OpICmpGt(l)) -> code.(p) <- JCode.OpICmpGt(realloc p l)
      | (p, JCode.OpICmpLe(l)) -> code.(p) <- JCode.OpICmpLe(realloc p l)
      | (p, JCode.OpACmpEq(l)) -> code.(p) <- JCode.OpACmpEq(realloc p l)
      | (p, JCode.OpACmpNe(l)) -> code.(p) <- JCode.OpACmpNe(realloc p l)
      | (p, JCode.OpGoto(l)) -> code.(p) <- JCode.OpGoto(realloc p l)
      | (p, JCode.OpJsr(l)) -> code.(p) <- JCode.OpJsr(realloc p l)
      | (p, JCode.OpLookupSwitch(d,cases)) ->
        let cases = List.map(fun(a,b)-> (a ,realloc p b)) cases in
        code.(p) <- JCode.OpLookupSwitch(realloc p d ,cases)
      | (p, JCode.OpTableSwitch (def, low, high, defs)) ->
        let defs = Array.map (realloc p) defs in
        code.(p) <- JCode.OpTableSwitch(realloc p def,low, high, defs)
      | (p, c) -> code.(p) <- c
    ) codes;
    let lines = if !lines = [] then None else Some (List.rev !lines) in
    let excs = List.map (fun exc ->
        {
          exc with
          JCode.e_start = realloc 0 exc.JCode.e_start;
          e_end = realloc 0 exc.e_end;
          e_handler = realloc 0 exc.e_handler
        }
    ) (List.rev !excs) in
    let throws = List.rev !throws in
    (code,lines, excs, throws)
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

      match class_or_interface with
      | "class" ->
        debug "class@.";
        (fun inners fields methods ->
          debug "gen@.";
          ignore (const !ctx (ConstClass name));
          {
            cversion = $1;
            constants = ctx_to_array !ctx;
            cpath = name;
            csuper = $4;
            cflags = JData.JSuper :: access;
            cinterfaces = $5;
            cfields = fields;
            cmethods = methods;
            cattributes = (* $2::$7::$8::$9::$11 @ *) $10;
            cinner_types = inners;
            ctypes = $6;
          }
        )
      | "interface" ->
        (fun inners fields methods ->
          ignore (const !ctx (ConstClass name));
          {
            cversion = $1;
            constants = ctx_to_array !ctx;
            cpath = name;
            csuper = $4;
            cflags = JData.JInterface :: access;
            cinterfaces = $5;
            cfields = fields;
            cmethods = methods;
            cattributes = (* $2::$7::$8::$9::$11 @ *) $10;
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
                if_attributes = f.cf_attributes;
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
              cm_attributes = attributes;
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
              am_attributes = attributes;
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
            i_other_attributes = $10;
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
      debug "bytecode@.";

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
      cn := $3;
      ("class",$2,$3)
    }
  | DINTERFACE access classname SEP
    {
      cn := $3;
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
      let (name, md) = split_method $3 in
      let (vts, ovt) = JParseSignature.parse_method_descriptor md in
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
          (*
          let fs = JBasics.make_fs $3 (JParseSignature.parse_field_descriptor $4) in
          let f = {
            cf_signature = fs;
            cf_class_signature = JBasics.make_cfs !cn fs;
            cf_generic_signature = None;
            cf_access = cf_access $2;
            cf_static = List.mem `Static $2;
            cf_synthetic = List.mem `Synthetic $2;
            cf_enum = List.mem `Enum $2;
            cf_kind = if List.mem `Final $2 then Final else NotFinal;
            cf_value = None;
            cf_transient = false;
            cf_annotations = [];
            cf_other_flags = [];
            cf_attributes = {
              synthetic = false; deprecated = false; other = []
            }
          } in
          (fs,f)
          *)
          { JData.jf_name = "aaaaaaaaaaaaaaa"; jf_kind = JData.JKField;
            jf_vmsignature = JData.TInt; jf_signature = JData.TInt;
            jf_throws = []; jf_types = []; jf_flags = [JData.JPublic];
            jf_attributes = []; jf_constant = None; jf_code = None
          }

        }
      | DFIELD access Word Word optional_default SEP
        {
          (*
          let fs = JBasics.make_fs $3 (JParseSignature.parse_field_descriptor $4) in
          let f = {
            cf_signature = fs;
            cf_class_signature = JBasics.make_cfs !cn fs;
            cf_generic_signature = None;
            cf_access = cf_access $2;
            cf_static = List.mem `Static $2;
            cf_synthetic = List.mem `Synthetic $2;
            cf_enum = List.mem `Enum $2;
            cf_kind = if List.mem `Final $2 then Final else NotFinal;
            cf_value = None;
            cf_transient = false;
            cf_annotations = [];
            cf_other_flags = [];
            cf_attributes = {
              synthetic = false; deprecated = false; other = []
            }
          } in
          (fs,f)
          *)
          let jsig = JReader.parse_signature $4 in
          { JData.jf_name = $3; jf_kind = JData.JKField;
            jf_vmsignature = jsig; jf_signature = jsig;
            jf_throws = []; jf_types = []; jf_flags = $2;
            jf_attributes = []; jf_constant = None; jf_code = None
          }
        }
      | DFIELD field_start field_exts endfield
        {
          { JData.jf_name = "ccccccccccccc"; jf_kind = JData.JKField;
            jf_vmsignature = JData.TInt; jf_signature = JData.TInt;
            jf_throws = []; jf_types = []; jf_flags = [JData.JPublic];
            jf_attributes = []; jf_constant = None; jf_code = None
          }
          (*let fs = JBasics.make_fs "" (JParseSignature.parse_field_descriptor $2) in
          let f = {
            cf_signature = fs;
            cf_class_signature = JBasics.make_cfs !cn fs;
            cf_generic_signature = None;
            cf_access = `Default;
            cf_static = false;
            cf_synthetic = false;
            cf_enum = false;
            cf_kind = NotFinal;
            cf_value = None;
            cf_transient = false;
            cf_annotations = [];
            cf_other_flags = [];
            cf_attributes = {
              synthetic = false; deprecated = false; other = []
            }
          } in
          (fs,f)
          *)
        }

      /* default value for a field */
      optional_default :
        | EQ item { Some $2 }
        | /* empty */ { None }

      /* multiline form of field description */
      field_start :
        | access Word Word optional_default SEP
          { "" }

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
  | inner_list { debug "inners@."; [] (*List.rev $1*) }
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
          let(access,(name,md)) = $1 in
          let code,lines,excs,throws = mkcode (List.rev $2) in
          let jmethod = {
            JCode.max_stack = !limit_stack;
            max_locals = !limit_locals;
            code = code;
            exc_tbl = excs;
            (*c_line_number_table = lines; TODO *)
            (*c_local_variable_table = None;  TODO *)
            (*c_local_variable_type_table = None;  TODO *)
            (*c_stack_map_midp = None;  TODO *)
            (*c_stack_map_java6 = None;  TODO *)
            attributes = []; (* TODO *)
          }
          in
          JCodeWriter.encode_code !ctx jmethod;
          let code = IO.close_out !ctx.ch in
          !ctx.ch <- !back_ch;
          Format.printf "name = %S code = %S@."  name code;
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
            cm_attributes = { synthetic = false; deprecated = false; other = [] };  *)(* TODO *)(*
            cm_annotations = { ma_global = []; ma_parameters = [] }; *)(* TODO *)(*
            cm_implementation = Java (lazy jmethod)
          } in (ms,m)
          *)
          { JData.jf_name = name;
            jf_kind = JData.JKMethod;
            jf_vmsignature = md;
            jf_signature = md;
            jf_throws = [];
            jf_types = [];
            jf_flags = access;
            jf_attributes = [JData.AttrUnknown ("Code", code)];
            jf_constant = None;
            jf_code = None
          }
        }
      | defmethod endmethod
        {
          let(access,(name,md)) = $1 in
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
            cm_attributes = { synthetic = false; deprecated = false; other = [] };  *)(* TODO *)(*
            cm_annotations = { ma_global = []; ma_parameters = [] }; *)(* TODO *)(*
            cm_implementation = Java (lazy jmethod)
          } in (ms,m)
          *)
          { JData.jf_name = name;
            jf_kind = JData.JKMethod;
            jf_vmsignature = md;
            jf_signature = md;
            jf_throws = [];
            jf_types = [];
            jf_flags = access;
            jf_attributes = [];
            jf_constant = None;
            jf_code = None
          }

        }

      defmethod :
        | DMETHOD access Word SEP
          {
            init_method();
            let (name, md) = split_method $3 in
            let md = JData.TMethod(JReader.parse_method_signature md) in
            (*
            let (vts, ovt) = JParseSignature.parse_method_descriptor md in
            let ms = JBasics.make_ms name vts ovt
            in ($2, ms)
            *)
            ($2,(name,md))
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
            | error { add 0 JCode.OpInvalid }
            | label { $1 }
            | /* empty */ { add 0 JCode.OpInvalid }

            /* label: */
            label :
              | Word COLON {
                addlabel $1 !pos;
                add 0 JCode.OpInvalid
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
              | DLIMIT limit_expr { add 0 JCode.OpInvalid }
              | DLINE line_expr { add 0 JCode.OpInvalid }
              | DTHROWS throws_expr { add 0 JCode.OpInvalid }
              | DCATCH catch_expr { add 0 JCode.OpInvalid }
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
                    excs := {
                      JCode.e_start = label2int $3;
                      e_end = label2int $5;
                      e_handler = label2int $7;
                      e_catch_type = Some $1
                    } :: !excs
                  }
                | classname FROM Int TO Int USING Int
                  {
                    excs := {
                      JCode.e_start = $3;
                      e_end = $5;
                      e_handler = $7;
                      e_catch_type = Some $1
                    } :: !excs
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
                  | "aaload" -> add 1 (JCode.OpAALoad)
                  | "aastore" -> add 1 (JCode.OpAAStore)
                  | "aconst_null" -> add 1 (JCode.OpAConstNull)
                  | "aload_0" -> add 1 (JCode.OpALoad 0)
                  | "aload_1" -> add 1 (JCode.OpALoad 1)
                  | "aload_2" -> add 1 (JCode.OpALoad 2)
                  | "aload_3" -> add 1 (JCode.OpALoad 3)
                  | "areturn" -> add 1 (JCode.OpAReturn)
                  | "arraylength" -> add 1 (JCode.OpArrayLength)
                  | "astore_0" -> add 1 (JCode.OpAStore 0)
                  | "astore_1" -> add 1 (JCode.OpAStore 1)
                  | "astore_2" -> add 1 (JCode.OpAStore 2)
                  | "astore_3" -> add 1 (JCode.OpAStore 3)
                  | "athrow" -> add 1 (JCode.OpThrow)
                  (* B *)
                  | "baload" ->add 1 (JCode.OpBALoad)
                  | "bastore" -> add 1 (JCode.OpBAStore)
                  | "breakpoint" -> add 1 (JCode.OpBreakpoint)
                  (* C *)
                  | "caload" ->add 1 (JCode.OpCALoad)
                  | "castore" -> add 1 (JCode.OpCAStore)
                  (* D *)
                  | "d2f" -> add 1 (JCode.OpD2F)
                  | "d2i" -> add 1 (JCode.OpD2I)
                  | "d2l" -> add 1 (JCode.OpD2L)
                  | "dadd" -> add 1 (JCode.OpAdd `Double)
                  | "daload" ->add 1 (JCode.OpArrayLoad `Double)
                  | "dastore" -> add 1 (JCode.OpArrayStore `Double)
                  | "dcmpg" -> add 1 (JCode.OpDCmpG)
                  | "dcmpl" -> add 1 (JCode.OpDCmpL)
                  | "dconst_0" -> add 1 (JCode.OpDConst( (0.)))
                  | "dconst_1" -> add 1 (JCode.OpDConst( (1.)))
                  | "ddiv" -> add 1 (JCode.OpDiv `Double)
                  | "dload_0" -> add 1 (JCode.OpLoad (`Double, 0))
                  | "dload_1" -> add 1 (JCode.OpLoad (`Double, 1))
                  | "dload_2" -> add 1 (JCode.OpLoad (`Double, 2))
                  | "dload_3" -> add 1 (JCode.OpLoad (`Double, 3))
                  | "dmul" -> add 1 (JCode.OpMult `Double)
                  | "dneg" -> add 1 (JCode.OpNeg `Double)
                  | "drem" -> add 1 (JCode.OpRem `Double)
                  | "dreturn" -> add 1 (JCode.OpReturn `Double)
                  | "dstore_0" -> add 1 (JCode.OpStore (`Double, 0))
                  | "dstore_1" -> add 1 (JCode.OpStore (`Double, 1))
                  | "dstore_2" -> add 1 (JCode.OpStore (`Double, 2))
                  | "dstore_3" -> add 1 (JCode.OpStore (`Double, 3))
                  | "dsub" -> add 1 (JCode.OpSub `Double)
                  | "dup" -> add 1 (JCode.OpDup)
                  | "dup2" -> add 1 (JCode.OpDup2)
                  | "dup2_x1" -> add 1 (JCode.OpDup2X1)
                  | "dup2_x2" -> add 1 (JCode.OpDup2X2)
                  | "dup_x1" -> add 1 (JCode.OpDupX1)
                  | "dup_x2" -> add 1 (JCode.OpDupX2)
                  (* F *)
                  | "f2i" -> add 1 (JCode.OpF2I)
                  | "f2l" -> add 1 (JCode.OpF2L)
                  | "f2d" -> add 1 (JCode.OpF2D)
                  | "fadd" -> add 1 (JCode.OpAdd `Float)
                  | "faload" ->add 1 (JCode.OpArrayLoad `Float)
                  | "fastore" -> add 1 (JCode.OpArrayStore `Float)
                  | "fcmpg" -> add 1 (JCode.OpFCmpG)
                  | "fcmpl" -> add 1 (JCode.OpFCmpL)
                  | "fconst_0" -> add 1(JCode.OpFConst( (0.)))
                  | "fconst_1" -> add 1(JCode.OpFConst( (1.)))
                  | "fconst_2" -> add 1(JCode.OpFConst( (2.)))
                  | "fdiv" -> add 1 (JCode.OpDiv `Float)
                  | "fload_0" -> add 1 (JCode.OpLoad (`Float, 0))
                  | "fload_1" -> add 1 (JCode.OpLoad (`Float, 1))
                  | "fload_2" -> add 1 (JCode.OpLoad (`Float, 2))
                  | "fload_3" -> add 1 (JCode.OpLoad (`Float, 3))
                  | "fmul" -> add 1 (JCode.OpMult `Float)
                  | "fneg" -> add 1 (JCode.OpNeg `Float)
                  | "frem" -> add 1 (JCode.OpRem `Float)
                  | "freturn" -> add 1 (JCode.OpReturn `Float)
                  | "fstore_0" -> add 1 (JCode.OpStore (`Float, 0))
                  | "fstore_1" -> add 1 (JCode.OpStore (`Float, 1))
                  | "fstore_2" -> add 1 (JCode.OpStore (`Float, 2))
                  | "fstore_3" -> add 1 (JCode.OpStore (`Float, 3))
                  | "fsub" -> add 1 (JCode.OpSub `Float)
                  (* I *)
                  | "i2f" -> add 1 (JCode.OpI2F)
                  | "i2d" -> add 1 (JCode.OpI2D)
                  | "i2l" -> add 1 (JCode.OpI2L)
                  | "iadd" -> add 1 (JCode.OpAdd `Int2Bool)
                  | "iaload" -> add 1 (JCode.OpArrayLoad `Int)
                  | "iand" -> add 1 (JCode.OpIAnd)
                  | "iastore" -> add 1 (JCode.OpArrayStore `Int)
                  | "iconst_0" -> add 1 (JCode.OpIConst( (0l)))
                  | "iconst_1" -> add 1 (JCode.OpIConst( (1l)))
                  | "iconst_2" -> add 1 (JCode.OpIConst( (2l)))
                  | "iconst_3" -> add 1 (JCode.OpIConst( (3l)))
                  | "iconst_4" -> add 1 (JCode.OpIConst( (4l)))
                  | "iconst_5" -> add 1 (JCode.OpIConst( (5l)))
                  | "iconst_m1" -> add 1 (JCode.OpIConst( (-1l)))
                  | "idiv" -> add 1 (JCode.OpDiv `Int2Bool)
                  | "iload_0" -> add 1 (JCode.OpLoad (`Int2Bool, 0))
                  | "iload_1" -> add 1 (JCode.OpLoad (`Int2Bool, 1))
                  | "iload_2" -> add 1 (JCode.OpLoad (`Int2Bool, 2))
                  | "iload_3" -> add 1 (JCode.OpLoad (`Int2Bool, 3))
                  | "imul" -> add 1 (JCode.OpMult `Int2Bool)
                  | "ineg" -> add 1 (JCode.OpNeg `Int2Bool)
                  | "int2byte" -> add 1 (JCode.OpI2B)
                  | "int2char" -> add 1 (JCode.OpI2C)
                  | "int2short" -> add 1 (JCode.OpI2S)
                  (*
                  | "invokedynamic", "method" -> add 1 ()
                  *)
                  | "ior" -> add 1 (JCode.OpIOr)
                  | "irem" -> add 1 (JCode.OpRem `Int2Bool)
                  | "ireturn" -> add 1 (JCode.OpReturn `Int2Bool)
                  | "ishl" -> add 1 (JCode.OpIShl)
                  | "ishr" -> add 1 (JCode.OpIShr)
                  | "istore_0" -> add 1 (JCode.OpStore (`Int2Bool, 0))
                  | "istore_1" -> add 1 (JCode.OpStore (`Int2Bool, 1))
                  | "istore_2" -> add 1 (JCode.OpStore (`Int2Bool, 2))
                  | "istore_3" -> add 1 (JCode.OpStore (`Int2Bool, 3))
                  | "isub" -> add 1 (JCode.OpSub `Int2Bool)
                  | "iushr" -> add 1 (JCode.OpIUShr)
                  | "ixor" -> add 1 (JCode.OpIXor)
                  (* L *)
                  | "l2f" -> add 1 (JCode.OpL2F)
                  | "l2d" -> add 1 (JCode.OpL2D)
                  | "l2i" -> add 1 (JCode.OpL2I)
                  | "ladd" -> add 1 (JCode.OpAdd `Long)
                  | "laload" ->add 1 (JCode.OpArrayLoad `Long)
                  | "land" -> add 1 (JCode.OpLAnd)
                  | "lastore" -> add 1 (JCode.OpArrayStore `Long)
                  | "lcmp" -> add 1 (JCode.OpLCmp)
                  | "lconst_0" -> add 1 (JCode.OpLConst( (Int64.of_int 0)))
                  | "lconst_1" -> add 1 (JCode.OpLConst( (Int64.of_int 1)))
                  | "ldiv" -> add 1 (JCode.OpDiv `Long)
                  | "lload_0" -> add 1 (JCode.OpLoad (`Long, 0))
                  | "lload_1" -> add 1 (JCode.OpLoad (`Long, 1))
                  | "lload_2" -> add 1 (JCode.OpLoad (`Long, 2))
                  | "lload_3" -> add 1 (JCode.OpLoad (`Long, 3))
                  | "lmul" -> add 1 (JCode.OpMult `Long)
                  | "lneg" -> add 1 (JCode.OpNeg `Long)
                  | "lor" -> add 1 (JCode.OpLOr)
                  | "lrem" -> add 1 (JCode.OpRem `Long)
                  | "lreturn" -> add 1 (JCode.OpReturn `Long)
                  | "lshl" -> add 1 (JCode.OpLShl)
                  | "lshr" -> add 1 (JCode.OpLShr)
                  | "lstore_0" -> add 1 (JCode.OpStore (`Long, 0))
                  | "lstore_1" -> add 1 (JCode.OpStore (`Long, 1))
                  | "lstore_2" -> add 1 (JCode.OpStore (`Long, 2))
                  | "lstore_3" -> add 1 (JCode.OpStore (`Long, 3))
                  | "lsub" -> add 1 (JCode.OpSub `Long)
                  | "lushr" -> add 1 (JCode.OpLUShr)
                  | "lxor" -> add 1 (JCode.OpLXor)
                  (* M *)
                  | "monitorenter" -> add 1 (JCode.OpMonitorEnter)
                  | "monitorexit" -> add 1 (JCode.OpMonitorExit)
                  (* N *)
                  | "nop" -> add 1 (JCode.OpNop)
                  (* P *)
                  | "pop" -> add 1 (JCode.OpPop)
                  | "pop2" -> add 1 (JCode.OpPop2)
                  (* R *)
                  | "return" -> add 1 (JCode.OpReturnVoid)
                  (* S *)
                  | "saload" -> add 1 (JCode.OpSALoad)
                  | "sastore" -> add 1 (JCode.OpSAStore)
                  | "swap" -> add 1 (JCode.OpSwap)
                  | a -> Printf.printf "Inst(%S, %S)@." a (snd $1); assert false
                }
                | Insn Int Int {
                  match(fst $1,snd $1, $2,$3)with
                  | "iinc", "ii", i1, i2 -> add 6 (JCode.OpIInc (i1, i2))
                  | "iinc", "Ii", i1, i2 -> add 6 (JCode.OpIInc (i1, i2))
                  | a,b,i1,i2 ->
                    Printf.printf "InstIntInt(%S, %S, %d, %d)@." a b i1 i2;
                    assert false
                }
                | Insn Int {
                  match(fst $1,snd $1, $2)with
                  (* A *)
                  | "aload", _, 0 -> add 1 (JCode.OpALoad 0)
                  | "aload", _, 1 -> add 1 (JCode.OpALoad 1)
                  | "aload", _, 2 -> add 1 (JCode.OpALoad 2)
                  | "aload", _, 3 -> add 1 (JCode.OpALoad 3)
                  | "aload", "i", n -> add 2 (JCode.OpALoad n)
                  | "aload", "I", n -> add 3 (JCode.OpALoad n)
                  | "astore", _, 0 -> add 1 (JCode.OpAStore 0)
                  | "astore", _, 1 -> add 1 (JCode.OpAStore 1)
                  | "astore", _, 2 -> add 1 (JCode.OpAStore 2)
                  | "astore", _, 3 -> add 1 (JCode.OpAStore 3)
                  | "astore", "i", n -> add 2 (JCode.OpAStore n)
                  | "astore", "I", n -> add 3 (JCode.OpAStore n)
                  (* B *)
                  | "bipush", "i", n -> add 2 (JCode.OpBIPush(n))
                  (* D *)
                  | "dload", _, 0 -> add 1 (JCode.OpLoad (`Double, 0))
                  | "dload", _, 1 -> add 1 (JCode.OpLoad (`Double, 1))
                  | "dload", _, 2 -> add 1 (JCode.OpLoad (`Double, 2))
                  | "dload", _, 3 -> add 1 (JCode.OpLoad (`Double, 3))
                  | "dload", "i", i -> add 2 (JCode.OpLoad (`Double, i))
                  | "dload", "I", n -> add 3 (JCode.OpLoad (`Double, n))
                  | "dstore", _, 0 -> add 1 (JCode.OpStore (`Double, 0))
                  | "dstore", _, 1 -> add 1 (JCode.OpStore (`Double, 1))
                  | "dstore", _, 2 -> add 1 (JCode.OpStore (`Double, 2))
                  | "dstore", _, 3 -> add 1 (JCode.OpStore (`Double, 3))
                  | "dstore", "i", i -> add 2 (JCode.OpStore (`Double, i))
                  | "dstore", "I", n -> add 3 (JCode.OpStore (`Double, n))
                  (* F *)
                  | "fload", _, 0 -> add 1 (JCode.OpLoad (`Float, 0))
                  | "fload", _, 1 -> add 1 (JCode.OpLoad (`Float, 1))
                  | "fload", _, 2 -> add 1 (JCode.OpLoad (`Float, 2))
                  | "fload", _, 3 -> add 1 (JCode.OpLoad (`Float, 3))
                  | "fload", "i", i -> add 2 (JCode.OpLoad (`Float, i))
                  | "fload", "I", n -> add 3 (JCode.OpLoad (`Float, n))
                  | "fstore", _, 0 -> add 1 (JCode.OpStore (`Float, 0))
                  | "fstore", _, 1 -> add 1 (JCode.OpStore (`Float, 1))
                  | "fstore", _, 2 -> add 1 (JCode.OpStore (`Float, 2))
                  | "fstore", _, 3 -> add 1 (JCode.OpStore (`Float, 3))
                  | "fstore", "i", i -> add 2 (JCode.OpStore (`Float, i))
                  | "fstore", "I", n -> add 3 (JCode.OpStore (`Float, n))
                  (* G *)
                  | "goto", "label", n -> add 3 (JCode.OpGoto n)
                  | "goto_w", "label", n -> add 3 (JCode.OpGoto n)
                  (* I *)
                  | "if_acmpeq", "label", n -> add 3 (JCode.OpACmpEq n)
                  | "if_acmpne", "label", n -> add 3 (JCode.OpACmpNe n)
                  | "if_icmpeq", "label", n -> add 3 (JCode.OpICmpEq n)
                  | "if_icmpge", "label", n -> add 3 (JCode.OpICmpGe n)
                  | "if_icmpgt", "label", n -> add 3 (JCode.OpICmpGt n)
                  | "if_icmple", "label", n -> add 3 (JCode.OpICmpLe n)
                  | "if_icmplt", "label", n -> add 3 (JCode.OpICmpLt n)
                  | "if_icmpne", "label", n -> add 3 (JCode.OpICmpNe n)
                  | "ifeq", "label", n -> add 3 (JCode.OpIfEq n)
                  | "ifge", "label", n -> add 3 (JCode.OpIfGe n)
                  | "ifgt", "label", n -> add 3 (JCode.OpIfGt n)
                  | "ifle", "label", n -> add 3 (JCode.OpIfLe n)
                  | "iflt", "label", n -> add 3 (JCode.OpIfLt n)
                  | "ifne", "label", n -> add 3 (JCode.OpIfNe n)
                  | "ifnonnull", "label", n -> add 3 (JCode.OpIfNonNull n)
                  | "ifnull", "label", n -> add 3 (JCode.OpIfNull n)
                  | "iload", _, 0 -> add 1 (JCode.OpLoad (`Int2Bool, 0))
                  | "iload", _, 1 -> add 1 (JCode.OpLoad (`Int2Bool, 1))
                  | "iload", _, 2 -> add 1 (JCode.OpLoad (`Int2Bool, 2))
                  | "iload", _, 3 -> add 1 (JCode.OpLoad (`Int2Bool, 3))
                  | "iload", "i", n -> add 2 (JCode.OpLoad (`Int2Bool, n))
                  | "iload", "I", n -> add 3 (JCode.OpLoad (`Int2Bool, n))
                  | "istore", _, 0 -> add 1 (JCode.OpStore (`Int2Bool, 0))
                  | "istore", _, 1 -> add 1 (JCode.OpStore (`Int2Bool, 1))
                  | "istore", _, 2 -> add 1 (JCode.OpStore (`Int2Bool, 2))
                  | "istore", _, 3 -> add 1 (JCode.OpStore (`Int2Bool, 3))
                  | "istore", "i", i -> add 2 (JCode.OpStore (`Int2Bool, i))
                  | "istore", "I", i -> add 3 (JCode.OpStore (`Int2Bool, i))
                  (* I *)
                  | "ldc", "constant", n -> add 2 (JCode.OpLdc1(const !ctx (ConstInt (Int32.of_int n))))
                  | "ldc2_w", "bigconstant", d -> add 3 (JCode.OpLConst( (Int64.of_int d)))
                  | "lload", _, 0 -> add 1 (JCode.OpLoad (`Long, 0))
                  | "lload", _, 1 -> add 1 (JCode.OpLoad (`Long, 1))
                  | "lload", _, 2 -> add 1 (JCode.OpLoad (`Long, 2))
                  | "lload", _, 3 -> add 1 (JCode.OpLoad (`Long, 3))
                  | "lload", "i", i -> add 2 (JCode.OpLoad (`Long, i)) 
                  | "lload", "I", n -> add 3 (JCode.OpLoad (`Long, n))
                  | "lstore", _, 0 -> add 1 (JCode.OpStore (`Long, 0))
                  | "lstore", _, 1 -> add 1 (JCode.OpStore (`Long, 1))
                  | "lstore", _, 2 -> add 1 (JCode.OpStore (`Long, 2))
                  | "lstore", _, 3 -> add 1 (JCode.OpStore (`Long, 3))
                  | "lstore", "i", l -> add 2 (JCode.OpStore (`Long, l))
                  | "lstore", "I", l -> add 3 (JCode.OpStore (`Long, l))
                  (* R *)
                  | "ret", "i", n -> add 2 (JCode.OpRet n)
                  | "ret", "I", n -> add 3 (JCode.OpRet n)
                  (* S *)
                  | "sipush", "i", n -> add 3 (JCode.OpSIPush n)
                  | a,b,i1 ->
                    Printf.printf "InstInt(%S, %S, %d)@." a b i1;
                    assert false
                }
                | Insn Num {
                  match(fst $1,snd $1, $2)with
                  (* I *)
                  | "ldc", "constant", s -> add 2 (JCode.OpLdc1(const !ctx (ConstFloat (float_of_string s))))
                  | "ldc2_w", "bigconstant", d -> add 3 (JCode.OpLdc2w(const !ctx (ConstDouble (float_of_string d))))
                  | a,b,s ->
                    Printf.printf "InstNum(%S, %S, %S)@." a b s;
                    assert false
                }
                | Insn Word { 
                  match(fst $1,snd $1, $2)with
                  (* A *)
                  | "anewarray", "class", o ->
                  add 3 (JCode.OpANewArray (const !ctx (ConstClass (JReader.expand_path o))))
                  (* C *)
                  | "checkcast", "class", w ->
                    add 3 (JCode.OpCheckCast (const !ctx (ConstClass (JReader.expand_path w))))
                  (* G *)
                  | "goto", "label", l -> add 3 (JCode.OpGoto(label2int l))
                  | "goto_w", "label", l -> add 3 (JCode.OpGoto(label2int l))
                  (* I *)
                  | "if_acmpeq", "label", l -> add 3 (JCode.OpACmpEq (label2int l))
                  | "if_acmpne", "label", l -> add 3 (JCode.OpACmpNe (label2int l))
                  | "if_icmpeq", "label", l -> add 3 (JCode.OpICmpEq (label2int l))
                  | "if_icmpge", "label", l -> add 3 (JCode.OpICmpGe (label2int l))
                  | "if_icmpgt", "label", l -> add 3 (JCode.OpICmpGt (label2int l))
                  | "if_icmple", "label", l -> add 3 (JCode.OpICmpLe (label2int l))
                  | "if_icmplt", "label", l -> add 3 (JCode.OpICmpLt (label2int l))
                  | "if_icmpne", "label", l -> add 3 (JCode.OpICmpNe (label2int l))
                  | "ifeq", "label", l -> add 3 (JCode.OpIfEq (label2int l))
                  | "ifge", "label", l -> add 3 (JCode.OpIfGe (label2int l))
                  | "ifgt", "label", l -> add 3 (JCode.OpIfGt (label2int l))
                  | "ifle", "label", l -> add 3 (JCode.OpIfLe (label2int l))
                  | "iflt", "label", l -> add 3 (JCode.OpIfLt (label2int l))
                  | "ifne", "label", l -> add 3 (JCode.OpIfNe (label2int l))
                  | "instanceof", "class", o -> add 3 (JCode.OpInstanceOf (const !ctx (ConstClass (JReader.expand_path o))))
                  | "invokenonvirtual", "method", m ->
                    debug "invoke method@.";
                    let (obj,f) = split_method m in
                    debug "koko1 %s@." obj;
                    let (name,o) = split_obj obj in
                    debug "koko2@.";
                    let jpath = JReader.expand_path name in
                    debug "koko3@.";
                    let f = JReader.parse_method_signature f in
                    debug "koko4@.";
                    let i = const !ctx (ConstMethod (jpath, o, f)) in
                    debug "invoke method %d@." i;
                    add 3 (JCode.OpInvokeNonVirtual i)
                  | "invokestatic", "method", m ->
                    let (obj,f) = split_method m in
                    let (name,o) = split_obj obj in
                    let jpath = JReader.expand_path name in
                    let f = JReader.parse_method_signature f in
                    let i = const !ctx (ConstMethod (jpath, o, f)) in
                    add 3 (JCode.OpInvokeStatic i)
                    (*
                    add 3(JCode.OpInvoke (`Static (JBasics.make_cn name), JBasics.make_ms o args r))
                    *)
                  | "invokevirtual", "method", m ->
                    let (obj,f) = split_method m in
                    let (name,o) = split_obj obj in
                    let jpath = JReader.expand_path name in
                    let f = JReader.parse_method_signature f in
                    let i = const !ctx (ConstMethod (jpath, o, f)) in
                    add 3 (JCode.OpInvokeVirtual i)
                    
                    (*add 3(JCode.OpInvoke (`Virtual ((JBasics.TClass (JBasics.make_cn name))), JBasics.make_ms o args r))*)
                  (* J *)
                  | "jsr","label",label -> add 3 (JCode.OpJsr((label2int label)))
                  | "jsr_w","label",label -> add 3 (JCode.OpJsr((label2int label)))
                  | "ldc", "constant", d -> add 2 (JCode.OpLdc1(const !ctx (ConstFloat (float_of_string d))))
                  | "ldc2_w", "bigconstant", d -> add 3 (JCode.OpLdc2w(const !ctx (ConstDouble (float_of_string d))))
                  (* N *)
                  | "new", "class", o -> add 3 (JCode.OpNew (const !ctx (ConstClass (JReader.expand_path (replace_dot o)))))
                  | "newarray", "atype", t ->
                    let a = java_basic_type_of_string t in
                    add 2 (JCode.OpNewArray a)
                  | a,b,s ->
                    Printf.printf "InstWord(%S, %S, %S)@." a b s;
                    assert false
                }
                | Insn Word Int {
                  match(fst $1,snd $1, $2,$3)with
                  (* I *)
                  | "invokeinterface", "interface", m, i2 ->
                    let (obj,f) = split_method m in
                    let (name,o) = split_obj obj in
                    let jpath = JReader.expand_path name in
                    let f = JReader.parse_method_signature f in
                    let i = const !ctx (ConstMethod (jpath, o, f)) in
                    add 5(JCode.OpInvokeInterface (i, i2))
                  (* M *)
                  | "multianewarray", "marray", t, i ->
                    add 4 (JCode.OpAMultiNewArray (const !ctx (ConstClass (JReader.expand_path t)), i))
                  | a,b,s,i ->
                    Printf.printf "InstWordInt(%S, %S, %S, %d)@." a b s i;
                    assert false
                }
                | Insn Word Word {
                  match(fst $1, $2,$3)with
                  | "getfield", cf, fd ->
                    let (c,f) = split_obj(cf) in
                    let jpath = JReader.expand_path c in
                    let fd = JReader.parse_signature fd in
                    add 3 (JCode.OpGetField (const !ctx (ConstField(jpath,f,fd))))
                  | "getstatic", cf, fd ->
                    let (c,f) = split_obj(cf) in
                    let jpath = JReader.expand_path c in
                    let fd = JReader.parse_signature fd in
                    add 3 (JCode.OpGetStatic (const !ctx (ConstField(jpath,f,fd))))
                  | "putfield", cf, fd ->
                    let (c,f) = split_obj(cf) in
                    let jpath = JReader.expand_path c in
                    let fd = JReader.parse_signature fd in
                    add 3 (JCode.OpPutField (const !ctx (ConstField(jpath,f,fd))))
                  | "putstatic", cf, fd ->
                    let (c,f) = split_obj(cf) in
                    let jpath = JReader.expand_path c in
                    let fd = JReader.parse_signature fd in
                    add 3 (JCode.OpPutStatic (const !ctx (ConstField(jpath,f,fd))))
                  | a, b, s2 ->
                    Printf.printf "InstWordWord(%S, %S, %S, %S)@." a b (snd $1) s2;
                    assert false
                }
                | Insn Str { 
                  match(fst $1,snd $1, unescape $2)with
                  | "ldc", "constant", s -> add 2 (JCode.OpLdc1(const !ctx(ConstString s)))
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
                        | DefaultInt i -> i
                        | DefaultWord l -> label2int l
                      in
                      let ls = List.map(function
                        | CaseIntInt(i, j)-> (Int32.of_int i, j)
                        | CaseIntWord(i, j) -> (Int32.of_int i, (label2int j))
                      ) $2 in
                      let padding_size = (4 - (!pos + 1) mod 4) mod 4 in
                      let n = 9 + padding_size + 8 * (List.length $2) in
                      add n (JCode.OpLookupSwitch (a, ls))

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
                        | DefaultInt i -> i
                        | DefaultWord l -> label2int l
                      in
                      let defs = List.map default2int defs in
                      let defs = Array.of_list defs in
                      let padding_size = (4 - ((!pos + 1) mod 4)) mod 4 in
                      let n = 13 + padding_size + 4 * (Array.length defs) in
                      let high = if high = -1 then Array.length defs - 1 + low else high in
                      add n (JCode.OpTableSwitch ((default2int def), Int32.of_int low, Int32.of_int high, defs))
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

