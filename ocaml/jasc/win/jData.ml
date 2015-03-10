let debug fmt = Format.printf fmt
let debug0 fmt =
  Format.kfprintf (fun ppf  -> ())
    (Format.formatter_of_buffer (Buffer.create 16)) fmt
type jpath = (string list* string)
type jversion = (int* int)
type unqualified_name = string
type jwildcard =
  | WExtends
  | WSuper
  | WNone
type jtype_argument =
  | TType of jwildcard* jsignature
  | TAny
and jsignature =
  | TByte
  | TChar
  | TDouble
  | TFloat
  | TInt
  | TLong
  | TShort
  | TBool
  | TObject of jpath* jtype_argument list
  | TObjectInner of string list* (string* jtype_argument list) list
  | TArray of jsignature* int option
  | TMethod of jmethod_signature
  | TTypeParameter of string
and jmethod_signature = (jsignature list* jsignature option)
type jsignatures = jsignature list
type reference_type =
  | RGetField
  | RGetStatic
  | RPutField
  | RPutStatic
  | RInvokeVirtual
  | RInvokeStatic
  | RInvokeSpecial
  | RNewInvokeSpecial
  | RInvokeInterface
type bootstrap_method = int
type jconstant =
  | ConstClass of jpath
  | ConstField of (jpath* unqualified_name* jsignature)
  | ConstMethod of (jpath* unqualified_name* jmethod_signature)
  | ConstInterfaceMethod of (jpath* unqualified_name* jmethod_signature)
  | ConstString of string
  | ConstInt of int32
  | ConstFloat of float
  | ConstLong of int64
  | ConstDouble of float
  | ConstNameAndType of unqualified_name* jsignature
  | ConstUtf8 of string
  | ConstMethodHandle of (reference_type* jconstant)
  | ConstMethodType of jmethod_signature
  | ConstInvokeDynamic of (bootstrap_method* unqualified_name* jsignature)
  | ConstUnusable
type jcode = unit
let pp_jcode fmt code = Format.fprintf fmt "()"
let show_jcode code = "()"
type jaccess_flag =
  | JPublic
  | JPrivate
  | JProtected
  | JStatic
  | JFinal
  | JSynchronized
  | JVolatile
  | JTransient
  | JSynthetic
  | JEnum
  | JUnusable
  | JSuper
  | JInterface
  | JAbstract
  | JAnnotation
  | JBridge
  | JVarArgs
  | JNative
  | JStrict
type jaccess = jaccess_flag list
type jtypes = (string* jsignature option* jsignature list) list
type jannotation =
  {
  ann_type: jsignature;
  ann_elements: (string* jannotation_value) list;}
and jannotation_value =
  | ValConst of int* jconstant
  | ValEnum of jsignature* string
  | ValClass of jsignature
  | ValAnnotation of jannotation
  | ValArray of jannotation_value list
type jattribute =
  | AttrDeprecated
  | AttrVisibleAnnotations of jannotation list
  | AttrInvisibleAnnotations of jannotation list
  | AttrUnknown of string* string
type jattributes = jattribute list
type jfield_kind =
  | JKField
  | JKMethod
type jfield =
  {
  jf_name: string;
  jf_kind: jfield_kind;
  jf_vmsignature: jsignature;
  jf_signature: jsignature;
  jf_throws: jsignature list;
  jf_types: jtypes;
  jf_flags: jaccess;
  jf_attributes: jattribute list;
  jf_constant: jconstant option;
  jf_code: jcode option;}
type jfields = jfield list
type utf8ref = int
type classref = int
type nametyperef = int
type dynref = int
type bootstrapref = int
type jconstant_raw =
  | KClass of utf8ref
  | KFieldRef of (classref* nametyperef)
  | KMethodRef of (classref* nametyperef)
  | KInterfaceMethodRef of (classref* nametyperef)
  | KString of utf8ref
  | KInt of int32
  | KFloat of float
  | KLong of int64
  | KDouble of float
  | KNameAndType of (utf8ref* utf8ref)
  | KUtf8String of string
  | KMethodHandle of (reference_type* dynref)
  | KMethodType of utf8ref
  | KInvokeDynamic of (bootstrapref* nametyperef)
  | KUnusable
type jclass =
  {
  cversion: jversion;
  constants: jconstant array;
  cpath: jpath;
  csuper: jsignature;
  cflags: jaccess;
  cinterfaces: jsignature list;
  cfields: jfield list;
  cmethods: jfield list;
  cattributes: jattribute list;
  cinner_types: (jpath* jpath option* string option* jaccess) list;
  ctypes: jtypes;}
let is_override_attrib =
  function
  | AttrVisibleAnnotations ann ->
      List.exists
        (function
         | { ann_type = TObject (("java"::"lang"::[],"Override"),[]) } ->
             true
         | _ -> false) ann
  | _ -> false
let is_override field = List.exists is_override_attrib field.jf_attributes
let path_s (pack,name) = Printf.sprintf "%s.%s" (String.concat "." pack) name
let rec s_sig =
  function
  | TByte  -> "byte"
  | TChar  -> "char"
  | TDouble  -> "double"
  | TFloat  -> "float"
  | TInt  -> "int"
  | TLong  -> "long"
  | TShort  -> "short"
  | TBool  -> "bool"
  | TObject (path,args) -> Printf.sprintf "%s%s" (path_s path) (s_args args)
  | TObjectInner (sl,sjargl) ->
      let sjargl = List.map (fun (s,arg)  -> s ^ (s_args arg)) sjargl in
      Printf.sprintf "%s.%s" (String.concat "." sl)
        (String.concat "." sjargl)
  | TArray (s,i) ->
      (match i with
       | None  -> Printf.sprintf "%s[]" (s_sig s)
       | Some i -> Printf.sprintf "%s[%d]" (s_sig s) i)
  | TMethod (sigs,sopt) ->
      let sigs = String.concat ", " (List.map s_sig sigs) in
      (match sopt with
       | None  -> Printf.sprintf "(%s)" sigs
       | Some s -> Printf.sprintf "%s (%s)" (s_sig s) sigs)
  | TTypeParameter s -> s
and s_args =
  function
  | [] -> ""
  | args ->
      let args =
        List.map
          (function
           | TAny  -> "*"
           | TType (WNone ,s) -> s_sig s
           | TType (WExtends ,s) -> "+" ^ (s_sig s)
           | TType (WSuper ,s) -> "-" ^ (s_sig s)) args in
      Printf.sprintf "<%s>" (String.concat ", " args)
let s_field f =
  Printf.sprintf "%s%s %s" (if is_override f then "override " else "")
    (s_sig f.jf_signature) f.jf_name
let s_fields fs =
  Printf.sprintf "{ \n\t%s\n}" (String.concat "\n\t" (List.map s_field fs))
