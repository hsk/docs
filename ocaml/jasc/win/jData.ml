type jpath = (string list* string)
let rec pp_jpath fmt (a0,a1) =
  Format.fprintf fmt "(@[<hov>";
  (((fun x  ->
       Format.fprintf fmt "[@[<hov>";
       ignore
         (List.fold_left
            (fun sep  ->
               fun x  ->
                 if sep then Format.fprintf fmt ";@ ";
                 (Format.fprintf fmt "%S") x;
                 true) false x);
       Format.fprintf fmt "@]]")) a0;
   Format.fprintf fmt ",@ ";
   (Format.fprintf fmt "%S") a1);
  Format.fprintf fmt "@])"
and show_jpath x = Format.asprintf "%a" pp_jpath x
type jversion = (int* int)
let rec pp_jversion fmt (a0,a1) =
  Format.fprintf fmt "(@[<hov>";
  ((Format.fprintf fmt "%d") a0;
   Format.fprintf fmt ",@ ";
   (Format.fprintf fmt "%d") a1);
  Format.fprintf fmt "@])"
and show_jversion x = Format.asprintf "%a" pp_jversion x
type unqualified_name = string
let rec pp_unqualified_name fmt = Format.fprintf fmt "%S"
and show_unqualified_name x = Format.asprintf "%a" pp_unqualified_name x
type jwildcard =
  | WExtends
  | WSuper
  | WNone
let rec pp_jwildcard fmt =
  function
  | WExtends  -> Format.pp_print_string fmt "JData.WExtends"
  | WSuper  -> Format.pp_print_string fmt "JData.WSuper"
  | WNone  -> Format.pp_print_string fmt "JData.WNone"
and show_jwildcard x = Format.asprintf "%a" pp_jwildcard x
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
let rec pp_jtype_argument fmt =
  function
  | TType (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>JData.TType (@,";
       ((pp_jwildcard fmt) a0;
        Format.fprintf fmt ",@ ";
        (pp_jsignature fmt) a1);
       Format.fprintf fmt "@])")
  | TAny  -> Format.pp_print_string fmt "JData.TAny"
and show_jtype_argument x = Format.asprintf "%a" pp_jtype_argument x
and pp_jsignature fmt =
  function
  | TByte  -> Format.pp_print_string fmt "JData.TByte"
  | TChar  -> Format.pp_print_string fmt "JData.TChar"
  | TDouble  -> Format.pp_print_string fmt "JData.TDouble"
  | TFloat  -> Format.pp_print_string fmt "JData.TFloat"
  | TInt  -> Format.pp_print_string fmt "JData.TInt"
  | TLong  -> Format.pp_print_string fmt "JData.TLong"
  | TShort  -> Format.pp_print_string fmt "JData.TShort"
  | TBool  -> Format.pp_print_string fmt "JData.TBool"
  | TObject (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>JData.TObject (@,";
       ((pp_jpath fmt) a0;
        Format.fprintf fmt ",@ ";
        ((fun x  ->
            Format.fprintf fmt "[@[<hov>";
            ignore
              (List.fold_left
                 (fun sep  ->
                    fun x  ->
                      if sep then Format.fprintf fmt ";@ ";
                      (pp_jtype_argument fmt) x;
                      true) false x);
            Format.fprintf fmt "@]]")) a1);
       Format.fprintf fmt "@])")
  | TObjectInner (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>JData.TObjectInner (@,";
       (((fun x  ->
            Format.fprintf fmt "[@[<hov>";
            ignore
              (List.fold_left
                 (fun sep  ->
                    fun x  ->
                      if sep then Format.fprintf fmt ";@ ";
                      (Format.fprintf fmt "%S") x;
                      true) false x);
            Format.fprintf fmt "@]]")) a0;
        Format.fprintf fmt ",@ ";
        ((fun x  ->
            Format.fprintf fmt "[@[<hov>";
            ignore
              (List.fold_left
                 (fun sep  ->
                    fun x  ->
                      if sep then Format.fprintf fmt ";@ ";
                      ((fun (a0,a1)  ->
                          Format.fprintf fmt "(@[<hov>";
                          ((Format.fprintf fmt "%S") a0;
                           Format.fprintf fmt ",@ ";
                           ((fun x  ->
                               Format.fprintf fmt "[@[<hov>";
                               ignore
                                 (List.fold_left
                                    (fun sep  ->
                                       fun x  ->
                                         if sep then Format.fprintf fmt ";@ ";
                                         (pp_jtype_argument fmt) x;
                                         true) false x);
                               Format.fprintf fmt "@]]")) a1);
                          Format.fprintf fmt "@])")) x;
                      true) false x);
            Format.fprintf fmt "@]]")) a1);
       Format.fprintf fmt "@])")
  | TArray (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>JData.TArray (@,";
       ((pp_jsignature fmt) a0;
        Format.fprintf fmt ",@ ";
        ((function
          | None  -> Format.pp_print_string fmt "None"
          | Some x ->
              (Format.pp_print_string fmt "(Some ";
               (Format.fprintf fmt "%d") x;
               Format.pp_print_string fmt ")"))) a1);
       Format.fprintf fmt "@])")
  | TMethod a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.TMethod@ ";
       (pp_jmethod_signature fmt) a0;
       Format.fprintf fmt "@])")
  | TTypeParameter a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.TTypeParameter@ ";
       (Format.fprintf fmt "%S") a0;
       Format.fprintf fmt "@])")
and show_jsignature x = Format.asprintf "%a" pp_jsignature x
and pp_jmethod_signature fmt (a0,a1) =
  Format.fprintf fmt "(@[<hov>";
  (((fun x  ->
       Format.fprintf fmt "[@[<hov>";
       ignore
         (List.fold_left
            (fun sep  ->
               fun x  ->
                 if sep then Format.fprintf fmt ";@ ";
                 (pp_jsignature fmt) x;
                 true) false x);
       Format.fprintf fmt "@]]")) a0;
   Format.fprintf fmt ",@ ";
   ((function
     | None  -> Format.pp_print_string fmt "None"
     | Some x ->
         (Format.pp_print_string fmt "(Some ";
          (pp_jsignature fmt) x;
          Format.pp_print_string fmt ")"))) a1);
  Format.fprintf fmt "@])"
and show_jmethod_signature x = Format.asprintf "%a" pp_jmethod_signature x
type jsignatures = jsignature list
let rec pp_jsignatures fmt x =
  Format.fprintf fmt "[@[<hov>";
  ignore
    (List.fold_left
       (fun sep  ->
          fun x  ->
            if sep then Format.fprintf fmt ";@ "; (pp_jsignature fmt) x; true)
       false x);
  Format.fprintf fmt "@]]"
and show_jsignatures x = Format.asprintf "%a" pp_jsignatures x
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
let rec pp_reference_type fmt =
  function
  | RGetField  -> Format.pp_print_string fmt "JData.RGetField"
  | RGetStatic  -> Format.pp_print_string fmt "JData.RGetStatic"
  | RPutField  -> Format.pp_print_string fmt "JData.RPutField"
  | RPutStatic  -> Format.pp_print_string fmt "JData.RPutStatic"
  | RInvokeVirtual  -> Format.pp_print_string fmt "JData.RInvokeVirtual"
  | RInvokeStatic  -> Format.pp_print_string fmt "JData.RInvokeStatic"
  | RInvokeSpecial  -> Format.pp_print_string fmt "JData.RInvokeSpecial"
  | RNewInvokeSpecial  ->
      Format.pp_print_string fmt "JData.RNewInvokeSpecial"
  | RInvokeInterface  -> Format.pp_print_string fmt "JData.RInvokeInterface"
and show_reference_type x = Format.asprintf "%a" pp_reference_type x
type bootstrap_method = int
let rec pp_bootstrap_method fmt = Format.fprintf fmt "%d"
and show_bootstrap_method x = Format.asprintf "%a" pp_bootstrap_method x
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
let rec pp_jconstant fmt =
  function
  | ConstClass a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.ConstClass@ ";
       (pp_jpath fmt) a0;
       Format.fprintf fmt "@])")
  | ConstField a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.ConstField@ ";
       ((fun (a0,a1,a2)  ->
           Format.fprintf fmt "(@[<hov>";
           (((pp_jpath fmt) a0;
             Format.fprintf fmt ",@ ";
             (pp_unqualified_name fmt) a1);
            Format.fprintf fmt ",@ ";
            (pp_jsignature fmt) a2);
           Format.fprintf fmt "@])")) a0;
       Format.fprintf fmt "@])")
  | ConstMethod a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.ConstMethod@ ";
       ((fun (a0,a1,a2)  ->
           Format.fprintf fmt "(@[<hov>";
           (((pp_jpath fmt) a0;
             Format.fprintf fmt ",@ ";
             (pp_unqualified_name fmt) a1);
            Format.fprintf fmt ",@ ";
            (pp_jmethod_signature fmt) a2);
           Format.fprintf fmt "@])")) a0;
       Format.fprintf fmt "@])")
  | ConstInterfaceMethod a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.ConstInterfaceMethod@ ";
       ((fun (a0,a1,a2)  ->
           Format.fprintf fmt "(@[<hov>";
           (((pp_jpath fmt) a0;
             Format.fprintf fmt ",@ ";
             (pp_unqualified_name fmt) a1);
            Format.fprintf fmt ",@ ";
            (pp_jmethod_signature fmt) a2);
           Format.fprintf fmt "@])")) a0;
       Format.fprintf fmt "@])")
  | ConstString a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.ConstString@ ";
       (Format.fprintf fmt "%S") a0;
       Format.fprintf fmt "@])")
  | ConstInt a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.ConstInt@ ";
       (Format.fprintf fmt "%ldl") a0;
       Format.fprintf fmt "@])")
  | ConstFloat a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.ConstFloat@ ";
       (Format.fprintf fmt "%F") a0;
       Format.fprintf fmt "@])")
  | ConstLong a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.ConstLong@ ";
       (Format.fprintf fmt "%LdL") a0;
       Format.fprintf fmt "@])")
  | ConstDouble a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.ConstDouble@ ";
       (Format.fprintf fmt "%F") a0;
       Format.fprintf fmt "@])")
  | ConstNameAndType (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>JData.ConstNameAndType (@,";
       ((pp_unqualified_name fmt) a0;
        Format.fprintf fmt ",@ ";
        (pp_jsignature fmt) a1);
       Format.fprintf fmt "@])")
  | ConstUtf8 a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.ConstUtf8@ ";
       (Format.fprintf fmt "%S") a0;
       Format.fprintf fmt "@])")
  | ConstMethodHandle a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.ConstMethodHandle@ ";
       ((fun (a0,a1)  ->
           Format.fprintf fmt "(@[<hov>";
           ((pp_reference_type fmt) a0;
            Format.fprintf fmt ",@ ";
            (pp_jconstant fmt) a1);
           Format.fprintf fmt "@])")) a0;
       Format.fprintf fmt "@])")
  | ConstMethodType a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.ConstMethodType@ ";
       (pp_jmethod_signature fmt) a0;
       Format.fprintf fmt "@])")
  | ConstInvokeDynamic a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.ConstInvokeDynamic@ ";
       ((fun (a0,a1,a2)  ->
           Format.fprintf fmt "(@[<hov>";
           (((pp_bootstrap_method fmt) a0;
             Format.fprintf fmt ",@ ";
             (pp_unqualified_name fmt) a1);
            Format.fprintf fmt ",@ ";
            (pp_jsignature fmt) a2);
           Format.fprintf fmt "@])")) a0;
       Format.fprintf fmt "@])")
  | ConstUnusable  -> Format.pp_print_string fmt "JData.ConstUnusable"
and show_jconstant x = Format.asprintf "%a" pp_jconstant x
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
let rec pp_jaccess_flag fmt =
  function
  | JPublic  -> Format.pp_print_string fmt "JData.JPublic"
  | JPrivate  -> Format.pp_print_string fmt "JData.JPrivate"
  | JProtected  -> Format.pp_print_string fmt "JData.JProtected"
  | JStatic  -> Format.pp_print_string fmt "JData.JStatic"
  | JFinal  -> Format.pp_print_string fmt "JData.JFinal"
  | JSynchronized  -> Format.pp_print_string fmt "JData.JSynchronized"
  | JVolatile  -> Format.pp_print_string fmt "JData.JVolatile"
  | JTransient  -> Format.pp_print_string fmt "JData.JTransient"
  | JSynthetic  -> Format.pp_print_string fmt "JData.JSynthetic"
  | JEnum  -> Format.pp_print_string fmt "JData.JEnum"
  | JUnusable  -> Format.pp_print_string fmt "JData.JUnusable"
  | JSuper  -> Format.pp_print_string fmt "JData.JSuper"
  | JInterface  -> Format.pp_print_string fmt "JData.JInterface"
  | JAbstract  -> Format.pp_print_string fmt "JData.JAbstract"
  | JAnnotation  -> Format.pp_print_string fmt "JData.JAnnotation"
  | JBridge  -> Format.pp_print_string fmt "JData.JBridge"
  | JVarArgs  -> Format.pp_print_string fmt "JData.JVarArgs"
  | JNative  -> Format.pp_print_string fmt "JData.JNative"
  | JStrict  -> Format.pp_print_string fmt "JData.JStrict"
and show_jaccess_flag x = Format.asprintf "%a" pp_jaccess_flag x
type jaccess = jaccess_flag list
let rec pp_jaccess fmt x =
  Format.fprintf fmt "[@[<hov>";
  ignore
    (List.fold_left
       (fun sep  ->
          fun x  ->
            if sep then Format.fprintf fmt ";@ ";
            (pp_jaccess_flag fmt) x;
            true) false x);
  Format.fprintf fmt "@]]"
and show_jaccess x = Format.asprintf "%a" pp_jaccess x
type jtypes = (string* jsignature option* jsignature list) list
let rec pp_jtypes fmt x =
  Format.fprintf fmt "[@[<hov>";
  ignore
    (List.fold_left
       (fun sep  ->
          fun x  ->
            if sep then Format.fprintf fmt ";@ ";
            ((fun (a0,a1,a2)  ->
                Format.fprintf fmt "(@[<hov>";
                (((Format.fprintf fmt "%S") a0;
                  Format.fprintf fmt ",@ ";
                  ((function
                    | None  -> Format.pp_print_string fmt "None"
                    | Some x ->
                        (Format.pp_print_string fmt "(Some ";
                         (pp_jsignature fmt) x;
                         Format.pp_print_string fmt ")"))) a1);
                 Format.fprintf fmt ",@ ";
                 ((fun x  ->
                     Format.fprintf fmt "[@[<hov>";
                     ignore
                       (List.fold_left
                          (fun sep  ->
                             fun x  ->
                               if sep then Format.fprintf fmt ";@ ";
                               (pp_jsignature fmt) x;
                               true) false x);
                     Format.fprintf fmt "@]]")) a2);
                Format.fprintf fmt "@])")) x;
            true) false x);
  Format.fprintf fmt "@]]"
and show_jtypes x = Format.asprintf "%a" pp_jtypes x
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
let rec pp_jannotation fmt x =
  Format.fprintf fmt "{ @[<hov>";
  ((Format.pp_print_string fmt "JData.ann_type = ";
    (pp_jsignature fmt) x.ann_type);
   Format.fprintf fmt ";@ ";
   Format.pp_print_string fmt "ann_elements = ";
   ((fun x  ->
       Format.fprintf fmt "[@[<hov>";
       ignore
         (List.fold_left
            (fun sep  ->
               fun x  ->
                 if sep then Format.fprintf fmt ";@ ";
                 ((fun (a0,a1)  ->
                     Format.fprintf fmt "(@[<hov>";
                     ((Format.fprintf fmt "%S") a0;
                      Format.fprintf fmt ",@ ";
                      (pp_jannotation_value fmt) a1);
                     Format.fprintf fmt "@])")) x;
                 true) false x);
       Format.fprintf fmt "@]]")) x.ann_elements);
  Format.fprintf fmt "@] }"
and show_jannotation x = Format.asprintf "%a" pp_jannotation x
and pp_jannotation_value fmt =
  function
  | ValConst (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>JData.ValConst (@,";
       ((Format.fprintf fmt "%d") a0;
        Format.fprintf fmt ",@ ";
        (pp_jconstant fmt) a1);
       Format.fprintf fmt "@])")
  | ValEnum (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>JData.ValEnum (@,";
       ((pp_jsignature fmt) a0;
        Format.fprintf fmt ",@ ";
        (Format.fprintf fmt "%S") a1);
       Format.fprintf fmt "@])")
  | ValClass a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.ValClass@ ";
       (pp_jsignature fmt) a0;
       Format.fprintf fmt "@])")
  | ValAnnotation a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.ValAnnotation@ ";
       (pp_jannotation fmt) a0;
       Format.fprintf fmt "@])")
  | ValArray a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.ValArray@ ";
       ((fun x  ->
           Format.fprintf fmt "[@[<hov>";
           ignore
             (List.fold_left
                (fun sep  ->
                   fun x  ->
                     if sep then Format.fprintf fmt ";@ ";
                     (pp_jannotation_value fmt) x;
                     true) false x);
           Format.fprintf fmt "@]]")) a0;
       Format.fprintf fmt "@])")
and show_jannotation_value x = Format.asprintf "%a" pp_jannotation_value x
type jattribute =
  | AttrDeprecated
  | AttrVisibleAnnotations of jannotation list
  | AttrInvisibleAnnotations of jannotation list
  | AttrUnknown of string* string
let rec pp_jattribute fmt =
  function
  | AttrDeprecated  -> Format.pp_print_string fmt "JData.AttrDeprecated"
  | AttrVisibleAnnotations a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.AttrVisibleAnnotations@ ";
       ((fun x  ->
           Format.fprintf fmt "[@[<hov>";
           ignore
             (List.fold_left
                (fun sep  ->
                   fun x  ->
                     if sep then Format.fprintf fmt ";@ ";
                     (pp_jannotation fmt) x;
                     true) false x);
           Format.fprintf fmt "@]]")) a0;
       Format.fprintf fmt "@])")
  | AttrInvisibleAnnotations a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.AttrInvisibleAnnotations@ ";
       ((fun x  ->
           Format.fprintf fmt "[@[<hov>";
           ignore
             (List.fold_left
                (fun sep  ->
                   fun x  ->
                     if sep then Format.fprintf fmt ";@ ";
                     (pp_jannotation fmt) x;
                     true) false x);
           Format.fprintf fmt "@]]")) a0;
       Format.fprintf fmt "@])")
  | AttrUnknown (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>JData.AttrUnknown (@,";
       ((Format.fprintf fmt "%S") a0;
        Format.fprintf fmt ",@ ";
        (Format.fprintf fmt "%S") a1);
       Format.fprintf fmt "@])")
and show_jattribute x = Format.asprintf "%a" pp_jattribute x
type jattributes = jattribute list
let rec pp_jattributes fmt x =
  Format.fprintf fmt "[@[<hov>";
  ignore
    (List.fold_left
       (fun sep  ->
          fun x  ->
            if sep then Format.fprintf fmt ";@ "; (pp_jattribute fmt) x; true)
       false x);
  Format.fprintf fmt "@]]"
and show_jattributes x = Format.asprintf "%a" pp_jattributes x
type jfield_kind =
  | JKField
  | JKMethod
let rec pp_jfield_kind fmt =
  function
  | JKField  -> Format.pp_print_string fmt "JData.JKField"
  | JKMethod  -> Format.pp_print_string fmt "JData.JKMethod"
and show_jfield_kind x = Format.asprintf "%a" pp_jfield_kind x
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
let rec pp_jfield fmt x =
  Format.fprintf fmt "{ @[<hov>";
  ((((((((((Format.pp_print_string fmt "JData.jf_name = ";
            (Format.fprintf fmt "%S") x.jf_name);
           Format.fprintf fmt ";@ ";
           Format.pp_print_string fmt "jf_kind = ";
           (pp_jfield_kind fmt) x.jf_kind);
          Format.fprintf fmt ";@ ";
          Format.pp_print_string fmt "jf_vmsignature = ";
          (pp_jsignature fmt) x.jf_vmsignature);
         Format.fprintf fmt ";@ ";
         Format.pp_print_string fmt "jf_signature = ";
         (pp_jsignature fmt) x.jf_signature);
        Format.fprintf fmt ";@ ";
        Format.pp_print_string fmt "jf_throws = ";
        ((fun x  ->
            Format.fprintf fmt "[@[<hov>";
            ignore
              (List.fold_left
                 (fun sep  ->
                    fun x  ->
                      if sep then Format.fprintf fmt ";@ ";
                      (pp_jsignature fmt) x;
                      true) false x);
            Format.fprintf fmt "@]]")) x.jf_throws);
       Format.fprintf fmt ";@ ";
       Format.pp_print_string fmt "jf_types = ";
       (pp_jtypes fmt) x.jf_types);
      Format.fprintf fmt ";@ ";
      Format.pp_print_string fmt "jf_flags = ";
      (pp_jaccess fmt) x.jf_flags);
     Format.fprintf fmt ";@ ";
     Format.pp_print_string fmt "jf_attributes = ";
     ((fun x  ->
         Format.fprintf fmt "[@[<hov>";
         ignore
           (List.fold_left
              (fun sep  ->
                 fun x  ->
                   if sep then Format.fprintf fmt ";@ ";
                   (pp_jattribute fmt) x;
                   true) false x);
         Format.fprintf fmt "@]]")) x.jf_attributes);
    Format.fprintf fmt ";@ ";
    Format.pp_print_string fmt "jf_constant = ";
    ((function
      | None  -> Format.pp_print_string fmt "None"
      | Some x ->
          (Format.pp_print_string fmt "(Some ";
           (pp_jconstant fmt) x;
           Format.pp_print_string fmt ")"))) x.jf_constant);
   Format.fprintf fmt ";@ ";
   Format.pp_print_string fmt "jf_code = ";
   ((function
     | None  -> Format.pp_print_string fmt "None"
     | Some x ->
         (Format.pp_print_string fmt "(Some ";
          (pp_jcode fmt) x;
          Format.pp_print_string fmt ")"))) x.jf_code);
  Format.fprintf fmt "@] }"
and show_jfield x = Format.asprintf "%a" pp_jfield x
type jfields = jfield list
let rec pp_jfields fmt x =
  Format.fprintf fmt "[@[<hov>";
  ignore
    (List.fold_left
       (fun sep  ->
          fun x  ->
            if sep then Format.fprintf fmt ";@ "; (pp_jfield fmt) x; true)
       false x);
  Format.fprintf fmt "@]]"
and show_jfields x = Format.asprintf "%a" pp_jfields x
type utf8ref = int
let rec pp_utf8ref fmt = Format.fprintf fmt "%d"
and show_utf8ref x = Format.asprintf "%a" pp_utf8ref x
type classref = int
let rec pp_classref fmt = Format.fprintf fmt "%d"
and show_classref x = Format.asprintf "%a" pp_classref x
type nametyperef = int
let rec pp_nametyperef fmt = Format.fprintf fmt "%d"
and show_nametyperef x = Format.asprintf "%a" pp_nametyperef x
type dynref = int
let rec pp_dynref fmt = Format.fprintf fmt "%d"
and show_dynref x = Format.asprintf "%a" pp_dynref x
type bootstrapref = int
let rec pp_bootstrapref fmt = Format.fprintf fmt "%d"
and show_bootstrapref x = Format.asprintf "%a" pp_bootstrapref x
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
let rec pp_jconstant_raw fmt =
  function
  | KClass a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.KClass@ ";
       (pp_utf8ref fmt) a0;
       Format.fprintf fmt "@])")
  | KFieldRef a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.KFieldRef@ ";
       ((fun (a0,a1)  ->
           Format.fprintf fmt "(@[<hov>";
           ((pp_classref fmt) a0;
            Format.fprintf fmt ",@ ";
            (pp_nametyperef fmt) a1);
           Format.fprintf fmt "@])")) a0;
       Format.fprintf fmt "@])")
  | KMethodRef a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.KMethodRef@ ";
       ((fun (a0,a1)  ->
           Format.fprintf fmt "(@[<hov>";
           ((pp_classref fmt) a0;
            Format.fprintf fmt ",@ ";
            (pp_nametyperef fmt) a1);
           Format.fprintf fmt "@])")) a0;
       Format.fprintf fmt "@])")
  | KInterfaceMethodRef a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.KInterfaceMethodRef@ ";
       ((fun (a0,a1)  ->
           Format.fprintf fmt "(@[<hov>";
           ((pp_classref fmt) a0;
            Format.fprintf fmt ",@ ";
            (pp_nametyperef fmt) a1);
           Format.fprintf fmt "@])")) a0;
       Format.fprintf fmt "@])")
  | KString a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.KString@ ";
       (pp_utf8ref fmt) a0;
       Format.fprintf fmt "@])")
  | KInt a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.KInt@ ";
       (Format.fprintf fmt "%ldl") a0;
       Format.fprintf fmt "@])")
  | KFloat a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.KFloat@ ";
       (Format.fprintf fmt "%F") a0;
       Format.fprintf fmt "@])")
  | KLong a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.KLong@ ";
       (Format.fprintf fmt "%LdL") a0;
       Format.fprintf fmt "@])")
  | KDouble a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.KDouble@ ";
       (Format.fprintf fmt "%F") a0;
       Format.fprintf fmt "@])")
  | KNameAndType a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.KNameAndType@ ";
       ((fun (a0,a1)  ->
           Format.fprintf fmt "(@[<hov>";
           ((pp_utf8ref fmt) a0;
            Format.fprintf fmt ",@ ";
            (pp_utf8ref fmt) a1);
           Format.fprintf fmt "@])")) a0;
       Format.fprintf fmt "@])")
  | KUtf8String a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.KUtf8String@ ";
       (Format.fprintf fmt "%S") a0;
       Format.fprintf fmt "@])")
  | KMethodHandle a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.KMethodHandle@ ";
       ((fun (a0,a1)  ->
           Format.fprintf fmt "(@[<hov>";
           ((pp_reference_type fmt) a0;
            Format.fprintf fmt ",@ ";
            (pp_dynref fmt) a1);
           Format.fprintf fmt "@])")) a0;
       Format.fprintf fmt "@])")
  | KMethodType a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.KMethodType@ ";
       (pp_utf8ref fmt) a0;
       Format.fprintf fmt "@])")
  | KInvokeDynamic a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.KInvokeDynamic@ ";
       ((fun (a0,a1)  ->
           Format.fprintf fmt "(@[<hov>";
           ((pp_bootstrapref fmt) a0;
            Format.fprintf fmt ",@ ";
            (pp_nametyperef fmt) a1);
           Format.fprintf fmt "@])")) a0;
       Format.fprintf fmt "@])")
  | KUnusable  -> Format.pp_print_string fmt "JData.KUnusable"
and show_jconstant_raw x = Format.asprintf "%a" pp_jconstant_raw x
type jclass =
  {
  cversion: jversion;
  constants: (jconstant_raw array* jconstant array);
  cpath: jpath;
  csuper: jsignature;
  cflags: jaccess;
  cinterfaces: jsignature list;
  cfields: jfield list;
  cmethods: jfield list;
  cattributes: jattribute list;
  cinner_types: (jpath* jpath option* string option* jaccess) list;
  ctypes: jtypes;}
let rec pp_jclass fmt x =
  Format.fprintf fmt "{ @[<hov>";
  (((((((((((Format.pp_print_string fmt "JData.cversion = ";
             (pp_jversion fmt) x.cversion);
            Format.fprintf fmt ";@ ";
            Format.pp_print_string fmt "constants = ";
            ((fun (a0,a1)  ->
                Format.fprintf fmt "(@[<hov>";
                (((fun x  ->
                     Format.fprintf fmt "[|@[<hov>";
                     ignore
                       (Array.fold_left
                          (fun sep  ->
                             fun x  ->
                               if sep then Format.fprintf fmt ";@ ";
                               (pp_jconstant_raw fmt) x;
                               true) false x);
                     Format.fprintf fmt "@]|]")) a0;
                 Format.fprintf fmt ",@ ";
                 ((fun x  ->
                     Format.fprintf fmt "[|@[<hov>";
                     ignore
                       (Array.fold_left
                          (fun sep  ->
                             fun x  ->
                               if sep then Format.fprintf fmt ";@ ";
                               (pp_jconstant fmt) x;
                               true) false x);
                     Format.fprintf fmt "@]|]")) a1);
                Format.fprintf fmt "@])")) x.constants);
           Format.fprintf fmt ";@ ";
           Format.pp_print_string fmt "cpath = ";
           (pp_jpath fmt) x.cpath);
          Format.fprintf fmt ";@ ";
          Format.pp_print_string fmt "csuper = ";
          (pp_jsignature fmt) x.csuper);
         Format.fprintf fmt ";@ ";
         Format.pp_print_string fmt "cflags = ";
         (pp_jaccess fmt) x.cflags);
        Format.fprintf fmt ";@ ";
        Format.pp_print_string fmt "cinterfaces = ";
        ((fun x  ->
            Format.fprintf fmt "[@[<hov>";
            ignore
              (List.fold_left
                 (fun sep  ->
                    fun x  ->
                      if sep then Format.fprintf fmt ";@ ";
                      (pp_jsignature fmt) x;
                      true) false x);
            Format.fprintf fmt "@]]")) x.cinterfaces);
       Format.fprintf fmt ";@ ";
       Format.pp_print_string fmt "cfields = ";
       ((fun x  ->
           Format.fprintf fmt "[@[<hov>";
           ignore
             (List.fold_left
                (fun sep  ->
                   fun x  ->
                     if sep then Format.fprintf fmt ";@ ";
                     (pp_jfield fmt) x;
                     true) false x);
           Format.fprintf fmt "@]]")) x.cfields);
      Format.fprintf fmt ";@ ";
      Format.pp_print_string fmt "cmethods = ";
      ((fun x  ->
          Format.fprintf fmt "[@[<hov>";
          ignore
            (List.fold_left
               (fun sep  ->
                  fun x  ->
                    if sep then Format.fprintf fmt ";@ ";
                    (pp_jfield fmt) x;
                    true) false x);
          Format.fprintf fmt "@]]")) x.cmethods);
     Format.fprintf fmt ";@ ";
     Format.pp_print_string fmt "cattributes = ";
     ((fun x  ->
         Format.fprintf fmt "[@[<hov>";
         ignore
           (List.fold_left
              (fun sep  ->
                 fun x  ->
                   if sep then Format.fprintf fmt ";@ ";
                   (pp_jattribute fmt) x;
                   true) false x);
         Format.fprintf fmt "@]]")) x.cattributes);
    Format.fprintf fmt ";@ ";
    Format.pp_print_string fmt "cinner_types = ";
    ((fun x  ->
        Format.fprintf fmt "[@[<hov>";
        ignore
          (List.fold_left
             (fun sep  ->
                fun x  ->
                  if sep then Format.fprintf fmt ";@ ";
                  ((fun (a0,a1,a2,a3)  ->
                      Format.fprintf fmt "(@[<hov>";
                      ((((pp_jpath fmt) a0;
                         Format.fprintf fmt ",@ ";
                         ((function
                           | None  -> Format.pp_print_string fmt "None"
                           | Some x ->
                               (Format.pp_print_string fmt "(Some ";
                                (pp_jpath fmt) x;
                                Format.pp_print_string fmt ")"))) a1);
                        Format.fprintf fmt ",@ ";
                        ((function
                          | None  -> Format.pp_print_string fmt "None"
                          | Some x ->
                              (Format.pp_print_string fmt "(Some ";
                               (Format.fprintf fmt "%S") x;
                               Format.pp_print_string fmt ")"))) a2);
                       Format.fprintf fmt ",@ ";
                       (pp_jaccess fmt) a3);
                      Format.fprintf fmt "@])")) x;
                  true) false x);
        Format.fprintf fmt "@]]")) x.cinner_types);
   Format.fprintf fmt ";@ ";
   Format.pp_print_string fmt "ctypes = ";
   (pp_jtypes fmt) x.ctypes);
  Format.fprintf fmt "@] }"
and show_jclass x = Format.asprintf "%a" pp_jclass x
let debug fmt = Format.printf fmt
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
