open JData

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

let rec pp_jversion fmt (a0,a1) =
  Format.fprintf fmt "(@[<hov>";
  ((Format.fprintf fmt "%d") a0;
   Format.fprintf fmt ",@ ";
   (Format.fprintf fmt "%d") a1);
  Format.fprintf fmt "@])"
and show_jversion x = Format.asprintf "%a" pp_jversion x

let rec pp_unqualified_name fmt = Format.fprintf fmt "%S"
and show_unqualified_name x = Format.asprintf "%a" pp_unqualified_name x

let rec pp_jwildcard fmt =
  function
  | WExtends  -> Format.pp_print_string fmt "JData.WExtends"
  | WSuper  -> Format.pp_print_string fmt "JData.WSuper"
  | WNone  -> Format.pp_print_string fmt "JData.WNone"
and show_jwildcard x = Format.asprintf "%a" pp_jwildcard x

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

let rec pp_bootstrap_method fmt = Format.fprintf fmt "%d"
and show_bootstrap_method x = Format.asprintf "%a" pp_bootstrap_method x

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

let pp_jcode fmt code = Format.fprintf fmt "()"
let show_jcode code = "()"

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

let rec pp_jaccess fmt x =
  Format.fprintf fmt "[@[<hov>";
  ignore
    (List.fold_left
       (fun sep x  ->
            if sep then Format.fprintf fmt ";@ ";
            (pp_jaccess_flag fmt) x;
            true) false x);
  Format.fprintf fmt "@]]"
and show_jaccess x = Format.asprintf "%a" pp_jaccess x

let rec pp_jtypes fmt x =
  Format.fprintf fmt "[@[<hov>";
  ignore
    (List.fold_left
       (fun sep x  ->
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
                          (fun sep x  ->
                               if sep then Format.fprintf fmt ";@ ";
                               (pp_jsignature fmt) x;
                               true) false x);
                     Format.fprintf fmt "@]]")) a2);
                Format.fprintf fmt "@])")) x;
            true) false x);
  Format.fprintf fmt "@]]"
and show_jtypes x = Format.asprintf "%a" pp_jtypes x

let rec pp_jannotation fmt x =
  Format.fprintf fmt "{ @[<hov>";
  ((Format.pp_print_string fmt "JData.ann_type = ";
    (pp_jsignature fmt) x.ann_type);
   Format.fprintf fmt ";@ ";
   Format.pp_print_string fmt "ann_elements = ";
   ((fun x  ->
       Format.fprintf fmt "[@[<hov>";
       ignore
         (List.fold_left (fun sep x ->
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
      Format.fprintf fmt "@[<hov2>JData.ValConst (@,";
       ((Format.fprintf fmt "%d") a0;
        Format.fprintf fmt ",@ ";
        (pp_jconstant fmt) a1);
      Format.fprintf fmt "@])"
  | ValEnum (a0,a1) ->
      Format.fprintf fmt "@[<hov2>JData.ValEnum (@,";
      pp_jsignature fmt a0;
      Format.fprintf fmt ",@ ";
      Format.fprintf fmt "%S" a1;
      Format.fprintf fmt "@])"
  | ValClass a0 ->
      Format.fprintf fmt "(@[<hov2>JData.ValClass@ ";
      pp_jsignature fmt a0;
      Format.fprintf fmt "@])"
  | ValAnnotation a0 ->
      Format.fprintf fmt "(@[<hov2>JData.ValAnnotation@ ";
      pp_jannotation fmt a0;
      Format.fprintf fmt "@])"
  | ValArray a0 ->
      Format.fprintf fmt "(@[<hov2>JData.ValArray@ ";
      Format.fprintf fmt "[@[<hov>";
      ignore (List.fold_left (fun sep x ->
        if sep then Format.fprintf fmt ";@ ";
        pp_jannotation_value fmt x;
        true
      ) false a0);
      Format.fprintf fmt "@]]";
      Format.fprintf fmt "@])"
and show_jannotation_value x = Format.asprintf "%a" pp_jannotation_value x

let rec pp_jattribute fmt =
  function
  | AttrDeprecated  -> Format.pp_print_string fmt "JData.AttrDeprecated"
  | AttrVisibleAnnotations a0 ->
      (Format.fprintf fmt "(@[<hov2>JData.AttrVisibleAnnotations@ ";
       ((fun x  ->
           Format.fprintf fmt "[@[<hov>";
           ignore
             (List.fold_left
                (fun sep x  ->
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
                (fun sep x  ->
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

let rec pp_jattributes fmt x =
  Format.fprintf fmt "[@[<hov>";
  ignore
    (List.fold_left
       (fun sep x  ->
            if sep then Format.fprintf fmt ";@ "; (pp_jattribute fmt) x; true)
       false x);
  Format.fprintf fmt "@]]"
and show_jattributes x = Format.asprintf "%a" pp_jattributes x

let rec pp_jfield_kind fmt =
  function
  | JKField  -> Format.pp_print_string fmt "JData.JKField"
  | JKMethod  -> Format.pp_print_string fmt "JData.JKMethod"
and show_jfield_kind x = Format.asprintf "%a" pp_jfield_kind x

let rec pp_jfield fmt x =
  Format.fprintf fmt "{ @[<hov>";
  Format.pp_print_string fmt "JData.jf_name = ";
            (Format.fprintf fmt "%S") x.jf_name;
           Format.fprintf fmt ";@ ";
           Format.pp_print_string fmt "jf_kind = ";
           (pp_jfield_kind fmt) x.jf_kind;
          Format.fprintf fmt ";@ ";
          Format.pp_print_string fmt "jf_vmsignature = ";
          (pp_jsignature fmt) x.jf_vmsignature;
         Format.fprintf fmt ";@ ";
         Format.pp_print_string fmt "jf_signature = ";
         (pp_jsignature fmt) x.jf_signature;
        Format.fprintf fmt ";@ ";
        Format.pp_print_string fmt "jf_throws = ";
        ((fun x  ->
            Format.fprintf fmt "[@[<hov>";
            ignore
              (List.fold_left
                 (fun sep x ->
                      if sep then Format.fprintf fmt ";@ ";
                      (pp_jsignature fmt) x;
                      true) false x);
            Format.fprintf fmt "@]]")) x.jf_throws;
       Format.fprintf fmt ";@ ";
       Format.pp_print_string fmt "jf_types = ";
       (pp_jtypes fmt) x.jf_types;
      Format.fprintf fmt ";@ ";
      Format.pp_print_string fmt "jf_flags = ";
      (pp_jaccess fmt) x.jf_flags;
     Format.fprintf fmt ";@ ";
     Format.pp_print_string fmt "jf_attributes = ";
     ((fun x  ->
         Format.fprintf fmt "[@[<hov>";
         ignore
           (List.fold_left
              (fun sep x  ->
                   if sep then Format.fprintf fmt ";@ ";
                   (pp_jattribute fmt) x;
                   true) false x);
         Format.fprintf fmt "@]]")) x.jf_attributes;
  Format.fprintf fmt ";@ ";
  Format.pp_print_string fmt "jf_constant = ";
  (function
      | None  -> Format.pp_print_string fmt "None"
      | Some x ->
          Format.pp_print_string fmt "(Some ";
          pp_jconstant fmt x;
           Format.pp_print_string fmt ")"
  ) x.jf_constant;
  Format.fprintf fmt ";@ ";
  Format.pp_print_string fmt "jf_code = ";
  (function
     | None  -> Format.pp_print_string fmt "None"
     | Some x ->
        Format.pp_print_string fmt "(Some ";
        pp_jcode fmt x;
        Format.pp_print_string fmt ")"
  ) x.jf_code;
  Format.fprintf fmt "@] }"
and show_jfield x = Format.asprintf "%a" pp_jfield x

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

let rec pp_utf8ref fmt = Format.fprintf fmt "%d"
and show_utf8ref x = Format.asprintf "%a" pp_utf8ref x

let rec pp_classref fmt = Format.fprintf fmt "%d"
and show_classref x = Format.asprintf "%a" pp_classref x

let rec pp_nametyperef fmt = Format.fprintf fmt "%d"
and show_nametyperef x = Format.asprintf "%a" pp_nametyperef x

let rec pp_dynref fmt = Format.fprintf fmt "%d"
and show_dynref x = Format.asprintf "%a" pp_dynref x

let rec pp_bootstrapref fmt = Format.fprintf fmt "%d"
and show_bootstrapref x = Format.asprintf "%a" pp_bootstrapref x

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
      Format.fprintf fmt "(@[<hov2>JData.KString@ ";
      pp_utf8ref fmt a0;
      Format.fprintf fmt "@])"
  | KInt a0 ->
      Format.fprintf fmt "(@[<hov2>JData.KInt@ ";
      Format.fprintf fmt "%ldl" a0;
      Format.fprintf fmt "@])"
  | KFloat a0 ->
      Format.fprintf fmt "(@[<hov2>JData.KFloat@ ";
      Format.fprintf fmt "%F" a0;
      Format.fprintf fmt "@])"
  | KLong a0 ->
      Format.fprintf fmt "(@[<hov2>JData.KLong@ ";
      Format.fprintf fmt "%LdL" a0;
      Format.fprintf fmt "@])"
  | KDouble a0 ->
      Format.fprintf fmt "(@[<hov2>JData.KDouble@ ";
      Format.fprintf fmt "%F" a0;
      Format.fprintf fmt "@])"
  | KNameAndType (a0,a1) ->
      Format.fprintf fmt "(@[<hov2>JData.KNameAndType@ ";
      Format.fprintf fmt "(@[<hov>";
      pp_utf8ref fmt a0;
      Format.fprintf fmt ",@ ";
      pp_utf8ref fmt a1;
      Format.fprintf fmt "@])";
      Format.fprintf fmt "@])"
  | KUtf8String a0 ->
      Format.fprintf fmt "(@[<hov2>JData.KUtf8String@ ";
      Format.fprintf fmt "%S" a0;
      Format.fprintf fmt "@])"
  | KMethodHandle (a0,a1) ->
      Format.fprintf fmt "(@[<hov2>JData.KMethodHandle@ ";
      Format.fprintf fmt "(@[<hov>";
      pp_reference_type fmt a0;
      Format.fprintf fmt ",@ ";
      pp_dynref fmt a1;
      Format.fprintf fmt "@])";
      Format.fprintf fmt "@])"
  | KMethodType a0 ->
      Format.fprintf fmt "(@[<hov2>JData.KMethodType@ ";
      pp_utf8ref fmt a0;
      Format.fprintf fmt "@])"
  | KInvokeDynamic (a0,a1) ->
      Format.fprintf fmt "(@[<hov2>JData.KInvokeDynamic@ ";
      Format.fprintf fmt "(@[<hov>";
      pp_bootstrapref fmt a0;
      Format.fprintf fmt ",@ ";
      pp_nametyperef fmt a1;
      Format.fprintf fmt "@])";
      Format.fprintf fmt "@])"
  | KUnusable  -> Format.pp_print_string fmt "JData.KUnusable"
and show_jconstant_raw x = Format.asprintf "%a" pp_jconstant_raw x

let rec pp_jclass fmt x =
  Format.fprintf fmt "{ @[<hov>";
  Format.pp_print_string fmt "JData.cversion = ";
  pp_jversion fmt x.cversion;
  Format.fprintf fmt ";@ ";
  Format.pp_print_string fmt "constants = ";
  Format.fprintf fmt "[|@[<hov>";
  Array.iteri (fun i x ->
    Format.fprintf fmt "%3d : %a;@ " i pp_jconstant x;
  ) x.constants;
  Format.fprintf fmt "@]|]";
  Format.fprintf fmt ";@ ";
  Format.pp_print_string fmt "cpath = ";
  pp_jpath fmt x.cpath;
  Format.fprintf fmt ";@ ";
  Format.pp_print_string fmt "csuper = ";
  pp_jsignature fmt x.csuper;
  Format.fprintf fmt ";@ ";
  Format.pp_print_string fmt "cflags = ";
  pp_jaccess fmt x.cflags;
  Format.fprintf fmt ";@ ";
  Format.pp_print_string fmt "cinterfaces = ";
  Format.fprintf fmt "[@[<hov>";
  ignore (List.fold_left (fun sep x ->
    if sep then Format.fprintf fmt ";@ ";
    pp_jsignature fmt x;
    true
  ) false x.cinterfaces);
  Format.fprintf fmt "@]]";
  Format.fprintf fmt ";@ ";
  Format.pp_print_string fmt "cfields = ";
  Format.fprintf fmt "[@[<hov>";
  ignore (List.fold_left (fun sep x  ->
    if sep then Format.fprintf fmt ";@ ";
    pp_jfield fmt x;
    true
  ) false x.cfields);
  Format.fprintf fmt "@]]";
  Format.fprintf fmt ";@ ";
  Format.pp_print_string fmt "cmethods = ";
      
  Format.fprintf fmt "[@[<hov>";
  ignore (List.fold_left (fun sep x  ->
    if sep then Format.fprintf fmt ";@ ";
    pp_jfield fmt x;
    true
  ) false x.cmethods);
  Format.fprintf fmt "@]]";
  Format.fprintf fmt ";@ ";
  Format.pp_print_string fmt "cattributes = ";
  Format.fprintf fmt "[@[<hov>";
  ignore (List.fold_left (fun sep x ->
    if sep then Format.fprintf fmt ";@ ";
    pp_jattribute fmt x;
    true
  ) false x.cattributes);
  Format.fprintf fmt "@]]";
  Format.fprintf fmt ";@ ";
  Format.pp_print_string fmt "cinner_types = ";
  Format.fprintf fmt "[@[<hov>";
  ignore (List.fold_left (fun sep (a0,a1,a2,a3)  ->
    if sep then Format.fprintf fmt ";@ ";
    Format.fprintf fmt "(@[<hov>";
    pp_jpath fmt a0;
    Format.fprintf fmt ",@ ";
    (match a1 with
    | None  -> Format.pp_print_string fmt "None"
    | Some x ->
      Format.pp_print_string fmt "(Some ";
      pp_jpath fmt x;
      Format.pp_print_string fmt ")"
    );
    Format.fprintf fmt ",@ ";
    (match a2 with
      | None  -> Format.pp_print_string fmt "None"
      | Some x ->
        Format.pp_print_string fmt "(Some ";
        Format.fprintf fmt "%S" x;
        Format.pp_print_string fmt ")"
    );
    Format.fprintf fmt ",@ ";
    pp_jaccess fmt a3;
    Format.fprintf fmt "@])";
    true
  ) false x.cinner_types);
  Format.fprintf fmt "@]]";
  Format.fprintf fmt ";@ ";
  Format.pp_print_string fmt "ctypes = ";
  pp_jtypes fmt x.ctypes;
  Format.fprintf fmt "@] }"

and show_jclass x = Format.asprintf "%a" pp_jclass x
