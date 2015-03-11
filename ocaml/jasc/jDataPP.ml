open JData

let rec pp_jpath fmt (a0,a1) =
  Format.fprintf fmt "(@[<hov>%a,@ %a@])"
    (fun fmt a0 ->
      Format.fprintf fmt "[@[<hov>";
      List.iter(Format.fprintf fmt "%S;@ ") a0;
      Format.fprintf fmt "@]]"
    ) a0
    (fun fmt -> Format.fprintf fmt "%S") a1
and show_jpath x = Format.asprintf "%a" pp_jpath x

let rec pp_jversion fmt (a0,a1) =
  Format.fprintf fmt "(@[<hov>%d,@ %d@])" a0 a1;
and show_jversion x = Format.asprintf "%a" pp_jversion x

let rec pp_unqualified_name fmt = Format.fprintf fmt "%S"
and show_unqualified_name x = Format.asprintf "%a" pp_unqualified_name x

let rec pp_jwildcard fmt =
  function
  | WExtends -> Format.pp_print_string fmt "JData.WExtends"
  | WSuper -> Format.pp_print_string fmt "JData.WSuper"
  | WNone -> Format.pp_print_string fmt "JData.WNone"
and show_jwildcard x = Format.asprintf "%a" pp_jwildcard x

let rec pp_jtype_argument fmt =
  function
  | TType (a0,a1) ->
      Format.fprintf fmt "@[<hov2>JData.TType (@,%a,@ %a@])"
        pp_jwildcard a0 pp_jsignature a1;
  | TAny -> Format.pp_print_string fmt "JData.TAny"
and show_jtype_argument x = Format.asprintf "%a" pp_jtype_argument x
and pp_jsignature fmt =
  function
  | TByte -> Format.pp_print_string fmt "JData.TByte"
  | TChar -> Format.pp_print_string fmt "JData.TChar"
  | TDouble -> Format.pp_print_string fmt "JData.TDouble"
  | TFloat -> Format.pp_print_string fmt "JData.TFloat"
  | TInt -> Format.pp_print_string fmt "JData.TInt"
  | TLong -> Format.pp_print_string fmt "JData.TLong"
  | TShort -> Format.pp_print_string fmt "JData.TShort"
  | TBool -> Format.pp_print_string fmt "JData.TBool"
  | TObject (a0,a1) ->
      Format.fprintf fmt "@[<hov2>JData.TObject (@,%a,@ %a@])"
        pp_jpath a0
        (fun fmt ->
          Format.fprintf fmt "[@[<hov>%a@]]"
            (fun fmt -> List.iter(Format.fprintf fmt "%a;@ " pp_jtype_argument))
        ) a1
  | TObjectInner (a0,a1) ->
      Format.fprintf fmt "@[<hov2>JData.TObjectInner (@,%a,@ %a@])"
        (fun fmt a0 ->
          Format.fprintf fmt "[@[<hov>";
          List.iter(Format.fprintf fmt "%S;@ ") a0;
          Format.fprintf fmt "@]]"
        ) a0
        (fun fmt a1 ->
          Format.fprintf fmt "[@[<hov>";
          List.iter (fun (a0,a1) ->
            Format.fprintf fmt "(@[<hov>%a,@ %a@]);@ "
              (fun fmt -> Format.fprintf fmt "%S,@ ") a0
              (fun fmt ->
                Format.fprintf fmt "[@[<hov>%a@]]"
                  (fun fmt -> List.iter(Format.fprintf fmt "%a;@ " pp_jtype_argument))
              ) a1
          ) a1;
          Format.fprintf fmt "@]]"
        ) a1
  | TArray (a0,a1) ->
      Format.fprintf fmt "@[<hov2>JData.TArray (@,%a,@ %a@])"
        pp_jsignature a0
        (fun fmt -> function
            | None -> Format.pp_print_string fmt "None"
            | Some x -> Format.fprintf fmt "(Some %d)" x
        ) a1
  | TMethod a0 ->
      Format.fprintf fmt "(@[<hov2>JData.TMethod@ %a@])"
        pp_jmethod_signature a0;
  | TTypeParameter a0 ->
      Format.fprintf fmt "(@[<hov2>JData.TTypeParameter@ %S@])" a0;
and show_jsignature x = Format.asprintf "%a" pp_jsignature x

and pp_jmethod_signature fmt (a0,a1) =
  Format.fprintf fmt "(@[<hov>";
  Format.fprintf fmt "[@[<hov>";
  List.iter(Format.fprintf fmt "%a;@ " pp_jsignature) a0;
  Format.fprintf fmt "@]],@ ";
  (function
     | None -> Format.pp_print_string fmt "None"
     | Some x -> Format.fprintf fmt "(Some %a)" pp_jsignature x
  ) a1;
  Format.fprintf fmt "@])"
and show_jmethod_signature x = Format.asprintf "%a" pp_jmethod_signature x

let rec pp_jsignatures fmt x =
  Format.fprintf fmt "[@[<hov>";
  List.iter(Format.fprintf fmt "%a;@ " pp_jsignature) x;
  Format.fprintf fmt "@]]"
and show_jsignatures x = Format.asprintf "%a" pp_jsignatures x

let rec pp_reference_type fmt =
  function
  | RGetField -> Format.pp_print_string fmt "JData.RGetField"
  | RGetStatic -> Format.pp_print_string fmt "JData.RGetStatic"
  | RPutField -> Format.pp_print_string fmt "JData.RPutField"
  | RPutStatic -> Format.pp_print_string fmt "JData.RPutStatic"
  | RInvokeVirtual -> Format.pp_print_string fmt "JData.RInvokeVirtual"
  | RInvokeStatic -> Format.pp_print_string fmt "JData.RInvokeStatic"
  | RInvokeSpecial -> Format.pp_print_string fmt "JData.RInvokeSpecial"
  | RNewInvokeSpecial -> Format.pp_print_string fmt "JData.RNewInvokeSpecial"
  | RInvokeInterface -> Format.pp_print_string fmt "JData.RInvokeInterface"
and show_reference_type x = Format.asprintf "%a" pp_reference_type x

let rec pp_bootstrap_method fmt = Format.fprintf fmt "%d"
and show_bootstrap_method x = Format.asprintf "%a" pp_bootstrap_method x

let rec pp_jconstant fmt =
  function
  | ConstClass a0 ->
      Format.fprintf fmt "(@[<hov2>JData.ConstClass@ %a@])" pp_jpath a0;
  | ConstField (a0,a1,a2) ->
      Format.fprintf fmt "(@[<hov2>JData.ConstField@ ";
      Format.fprintf fmt "(@[<hov>%a,@ %a,@ %a@])"
        pp_jpath a0 pp_unqualified_name a1 pp_jsignature a2;
      Format.fprintf fmt "@])"
  | ConstMethod (a0,a1,a2) ->
      Format.fprintf fmt "(@[<hov2>JData.ConstMethod@ ";
      Format.fprintf fmt "(@[<hov>%a,@ %a,@ %a@])"
        pp_jpath a0 pp_unqualified_name a1 pp_jmethod_signature a2;
      Format.fprintf fmt "@])"
  | ConstInterfaceMethod (a0,a1,a2) ->
      Format.fprintf fmt "(@[<hov2>JData.ConstInterfaceMethod@ ";
      Format.fprintf fmt "(@[<hov>%a,@ %a,@ %a@])"
        pp_jpath a0 pp_unqualified_name a1 pp_jmethod_signature a2;
      Format.fprintf fmt "@])"
  | ConstString a0 -> Format.fprintf fmt "(@[<hov2>JData.ConstString@ %S@])" a0;
  | ConstInt a0 -> Format.fprintf fmt "(@[<hov2>JData.ConstInt@ %ldl@])" a0;
  | ConstFloat a0 -> Format.fprintf fmt "(@[<hov2>JData.ConstFloat@ %F@])" a0;
  | ConstLong a0 -> Format.fprintf fmt "(@[<hov2>JData.ConstLong@ %LdL@])" a0;
  | ConstDouble a0 -> Format.fprintf fmt "(@[<hov2>JData.ConstDouble@ %F@])" a0;
  | ConstNameAndType (a0,a1) ->
      Format.fprintf fmt "@[<hov2>JData.ConstNameAndType (@,%a,@ %a@])"
        pp_unqualified_name a0 pp_jsignature a1;
  | ConstUtf8 a0 ->
      Format.fprintf fmt "(@[<hov2>JData.ConstUtf8@ %S@])" a0;
  | ConstMethodHandle (a0,a1) ->
      Format.fprintf fmt "(@[<hov2>JData.ConstMethodHandle@ ";
      Format.fprintf fmt "(@[<hov>%a,@ %a@])" pp_reference_type a0 pp_jconstant a1;
      Format.fprintf fmt "@])"
  | ConstMethodType a0 ->
      Format.fprintf fmt "(@[<hov2>JData.ConstMethodType@ %a@])"
        pp_jmethod_signature a0;
  | ConstInvokeDynamic (a0,a1,a2) ->
      Format.fprintf fmt "(@[<hov2>JData.ConstInvokeDynamic@ ";
      Format.fprintf fmt "(@[<hov>%a,@ %a,@ %a@])"
        pp_bootstrap_method a0 pp_unqualified_name a1 pp_jsignature a2;
      Format.fprintf fmt "@])"
  | ConstUnusable -> Format.pp_print_string fmt "JData.ConstUnusable"
and show_jconstant x = Format.asprintf "%a" pp_jconstant x

let pp_jcode fmt code = Format.fprintf fmt "()"
let show_jcode code = "()"

let rec pp_jaccess_flag fmt =
  function
  | JPublic -> Format.pp_print_string fmt "JData.JPublic"
  | JPrivate -> Format.pp_print_string fmt "JData.JPrivate"
  | JProtected -> Format.pp_print_string fmt "JData.JProtected"
  | JStatic -> Format.pp_print_string fmt "JData.JStatic"
  | JFinal -> Format.pp_print_string fmt "JData.JFinal"
  | JSynchronized -> Format.pp_print_string fmt "JData.JSynchronized"
  | JVolatile -> Format.pp_print_string fmt "JData.JVolatile"
  | JTransient -> Format.pp_print_string fmt "JData.JTransient"
  | JSynthetic -> Format.pp_print_string fmt "JData.JSynthetic"
  | JEnum -> Format.pp_print_string fmt "JData.JEnum"
  | JUnusable -> Format.pp_print_string fmt "JData.JUnusable"
  | JSuper -> Format.pp_print_string fmt "JData.JSuper"
  | JInterface -> Format.pp_print_string fmt "JData.JInterface"
  | JAbstract -> Format.pp_print_string fmt "JData.JAbstract"
  | JAnnotation -> Format.pp_print_string fmt "JData.JAnnotation"
  | JBridge -> Format.pp_print_string fmt "JData.JBridge"
  | JVarArgs -> Format.pp_print_string fmt "JData.JVarArgs"
  | JNative -> Format.pp_print_string fmt "JData.JNative"
  | JStrict -> Format.pp_print_string fmt "JData.JStrict"
and show_jaccess_flag x = Format.asprintf "%a" pp_jaccess_flag x

let rec pp_jaccess fmt x =
  Format.fprintf fmt "[@[<hov>%a@]]"
    (fun fmt ->
      List.iter (Format.fprintf fmt "%a;@ " pp_jaccess_flag)
    ) x
and show_jaccess x = Format.asprintf "%a" pp_jaccess x

let rec pp_jtypes fmt x =
  Format.fprintf fmt "[@[<hov>%a@]]"
    (fun fmt ->
      List.iter (fun (a0,a1,a2) ->
        Format.fprintf fmt "(@[<hov>%S,@ %a,@ %a@]);@ "
          a0
          (fun fmt -> function
              | None -> Format.pp_print_string fmt "None"
              | Some x -> Format.fprintf fmt "(Some %a)" pp_jsignature x
          ) a1
          (fun fmt ->
            Format.fprintf fmt "[@[<hov>%a@]]"
              (fun fmt ->
                List.iter (Format.fprintf fmt "%a;@ " pp_jsignature)
              )
          ) a2;
      )
    ) x
and show_jtypes x = Format.asprintf "%a" pp_jtypes x

let rec pp_jannotation fmt x =
  Format.fprintf fmt "{ @[<hov>";
  Format.fprintf fmt "JData.ann_type = %a;@ " pp_jsignature x.ann_type;
  Format.fprintf fmt "ann_elements = [@[<hov>%a@]]@ "
    (fun fmt ->
      List.iter (fun (a0, a1) ->
        Format.fprintf fmt "(@[<hov>%S,@ %a@]);@ " a0 pp_jannotation_value a1
      )
    ) x.ann_elements;
  Format.fprintf fmt "@] }"
and show_jannotation x = Format.asprintf "%a" pp_jannotation x
and pp_jannotation_value fmt =
  function
  | ValConst (a0,a1) ->
      Format.fprintf fmt "@[<hov2>JData.ValConst (@,%d,@ %a@])" a0 pp_jconstant a1
  | ValEnum (a0,a1) ->
      Format.fprintf fmt "@[<hov2>JData.ValEnum (@,%a,@ %S@])" pp_jsignature a0 a1
  | ValClass a0 ->
      Format.fprintf fmt "(@[<hov2>JData.ValClass@ %a@])" pp_jsignature a0;
  | ValAnnotation a0 ->
      Format.fprintf fmt "(@[<hov2>JData.ValAnnotation@ %a@])" pp_jannotation a0
  | ValArray a0 ->
      Format.fprintf fmt "(@[<hov2>JData.ValArray@ ";
      Format.fprintf fmt "[@[<hov>%a@]]"
        (fun fmt ->
          List.iter (Format.fprintf fmt "%a;@ " pp_jannotation_value)
        ) a0;
      Format.fprintf fmt "@])"
and show_jannotation_value x = Format.asprintf "%a" pp_jannotation_value x

let rec pp_jattribute fmt = function
  | AttrDeprecated -> Format.pp_print_string fmt "JData.AttrDeprecated"
  | AttrVisibleAnnotations a0 ->
      Format.fprintf fmt "(@[<hov2>JData.AttrVisibleAnnotations@ %a@])"
        (fun fmt ->
          Format.fprintf fmt "[@[<hov>%a@]]"
            (fun fmt ->
              List.iter (Format.fprintf fmt "%a;@ " pp_jannotation)
            )
        ) a0;
  | AttrInvisibleAnnotations a0 ->
      Format.fprintf fmt "(@[<hov2>JData.AttrInvisibleAnnotations@ ";
      Format.fprintf fmt "[@[<hov>%a@]]"
        (fun fmt ->
          List.iter (Format.fprintf fmt "%a;@ " pp_jannotation)
        ) a0;
      Format.fprintf fmt "@])"
  | AttrUnknown (a0,a1) ->
      Format.fprintf fmt "@[<hov2>JData.AttrUnknown (@,%S,@ %S@])" a0 a1
and show_jattribute x = Format.asprintf "%a" pp_jattribute x

let rec pp_jattributes fmt x =
  Format.fprintf fmt "[@[<hov>";
  List.iter (Format.fprintf fmt "%a;@ " pp_jattribute) x;
  Format.fprintf fmt "@]]"
and show_jattributes x = Format.asprintf "%a" pp_jattributes x

let rec pp_jfield_kind fmt = function
  | JKField -> Format.pp_print_string fmt "JData.JKField"
  | JKMethod -> Format.pp_print_string fmt "JData.JKMethod"
and show_jfield_kind x = Format.asprintf "%a" pp_jfield_kind x

let rec pp_jfield fmt x =
  Format.fprintf fmt "{ @[<hov>";
  Format.fprintf fmt "JData.jf_name = %S;@ " x.jf_name;
  Format.fprintf fmt "jf_kind = %a;@ " pp_jfield_kind x.jf_kind;
  Format.fprintf fmt "jf_vmsignature = %a;@ " pp_jsignature x.jf_vmsignature;
  Format.fprintf fmt "jf_signature = %a;@ " pp_jsignature x.jf_signature;
  Format.fprintf fmt "jf_throws = [@[<hov>%a@]];@ "
    (fun fmt ->
      List.iter (Format.fprintf fmt "%a;@ " pp_jsignature)
    ) x.jf_throws;
  Format.fprintf fmt "jf_types = %a;@ " pp_jtypes x.jf_types;
  Format.fprintf fmt "jf_flags = %a;@ " pp_jaccess x.jf_flags;
  Format.fprintf fmt "jf_attributes = [@[<hov>%a@]];@ "
    (fun fmt ->
      List.iter (Format.fprintf fmt "%a;@ " pp_jattribute)
    ) x.jf_attributes;
  Format.fprintf fmt "jf_constant = %a;@ "
    (fun fmt -> function
        | None -> Format.pp_print_string fmt "None"
        | Some x -> Format.fprintf fmt "(Some %a)" pp_jconstant x
    ) x.jf_constant;
  Format.fprintf fmt "jf_code = %a"
    (fun fmt -> function
       | None -> Format.pp_print_string fmt "None"
       | Some x -> Format.fprintf fmt "(Some %a)" pp_jcode x
    ) x.jf_code;
  Format.fprintf fmt "@] }"
and show_jfield x = Format.asprintf "%a" pp_jfield x

let rec pp_jfields fmt x =
  Format.fprintf fmt "[@[<hov>";
  List.iter (Format.fprintf fmt "%a;@ " pp_jfield) x;
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
      Format.fprintf fmt "(@[<hov2>JData.KClass@ %a@])" pp_utf8ref a0;
  | KFieldRef (a0, a1) ->
      Format.fprintf fmt "(@[<hov2>JData.KFieldRef@ ";
      Format.fprintf fmt "(@[<hov>%a,@ %a@])" pp_classref a0 pp_nametyperef a1;
      Format.fprintf fmt "@])"
  | KMethodRef (a0,a1) ->
      Format.fprintf fmt "(@[<hov2>JData.KMethodRef@ ";
      Format.fprintf fmt "(@[<hov>%a,@ %a@])" pp_classref a0 pp_nametyperef a1;
      Format.fprintf fmt "@])"
  | KInterfaceMethodRef (a0, a1) ->
      Format.fprintf fmt "(@[<hov2>JData.KInterfaceMethodRef@ ";
      Format.fprintf fmt "(@[<hov>%a,@ %a@])" pp_classref a0 pp_nametyperef a1;
      Format.fprintf fmt "@])"
  | KString a0 ->
      Format.fprintf fmt "(@[<hov2>JData.KString@ %a@])" pp_utf8ref a0;
  | KInt a0 ->
      Format.fprintf fmt "(@[<hov2>JData.KInt@ %ldl@])" a0;
  | KFloat a0 ->
      Format.fprintf fmt "(@[<hov2>JData.KFloat@ %F@])" a0;
  | KLong a0 ->
      Format.fprintf fmt "(@[<hov2>JData.KLong@ %LdL@])" a0;
  | KDouble a0 ->
      Format.fprintf fmt "(@[<hov2>JData.KDouble@ %F@])" a0;
  | KNameAndType (a0,a1) ->
      Format.fprintf fmt "(@[<hov2>JData.KNameAndType@ ";
      Format.fprintf fmt "(@[<hov>%a,@ %a@])" pp_utf8ref a0 pp_utf8ref a1;
      Format.fprintf fmt "@])"
  | KUtf8String a0 ->
      Format.fprintf fmt "(@[<hov2>JData.KUtf8String@ %S@])" a0;
  | KMethodHandle (a0,a1) ->
      Format.fprintf fmt "(@[<hov2>JData.KMethodHandle@ ";
      Format.fprintf fmt "(@[<hov>%a,@ %a@])" pp_reference_type a0 pp_dynref a1;
      Format.fprintf fmt "@])"
  | KMethodType a0 ->
      Format.fprintf fmt "(@[<hov2>JData.KMethodType@ %a@])" pp_utf8ref a0;
  | KInvokeDynamic (a0,a1) ->
      Format.fprintf fmt "(@[<hov2>JData.KInvokeDynamic@ ";
      Format.fprintf fmt "(@[<hov>%a,@ %a@])" pp_bootstrapref a0 pp_nametyperef a1;
      Format.fprintf fmt "@])"
  | KUnusable -> Format.pp_print_string fmt "JData.KUnusable"
and show_jconstant_raw x = Format.asprintf "%a" pp_jconstant_raw x

let rec pp_jclass fmt x =
  Format.fprintf fmt "{ @[<hov>";
  Format.fprintf fmt "JData.cversion = %a;@ " pp_jversion x.cversion;
  Format.fprintf fmt "constants = [|@[<hov>%a@]|];@ "
    (fun fmt ->
      Array.iteri (fun i x ->
        Format.fprintf fmt "(* %3d *) %a;@ " i pp_jconstant x
      )
    ) x.constants;
  Format.fprintf fmt "cpath = %a;@ " pp_jpath x.cpath;
  Format.fprintf fmt "csuper = %a;@ " pp_jsignature x.csuper;
  Format.fprintf fmt "cflags = %a;@ " pp_jaccess x.cflags;
  Format.fprintf fmt "cinterfaces = [@[<hov>%a@]];@ "
    (fun fmt ->
      List.iter (Format.fprintf fmt "%a;@ " pp_jsignature)
    ) x.cinterfaces;
  Format.fprintf fmt "cfields = [@[<hov>%a@]];@ "
    (fun fmt ->
      List.iter (Format.fprintf fmt "%a;@ " pp_jfield)
    ) x.cfields;
  Format.fprintf fmt "cmethods = [@[<hov>%a@]];@ "
    (fun fmt ->
      List.iter (Format.fprintf fmt "%a;@ " pp_jfield)
    ) x.cmethods;
  Format.fprintf fmt "cattributes = [@[<hov>%a@]];@ "
    (fun fmt ->
      List.iter (Format.fprintf fmt "%a;@ " pp_jattribute)
    ) x.cattributes;
  Format.fprintf fmt "cinner_types = [@[<hov>%a@]];@ "
    (fun fmt ->
      List.iter (fun (a0,a1,a2,a3) ->
        Format.fprintf fmt "(@[<hov>%a,@ %a,@ %a,@ %a@]);@ "
          pp_jpath a0
          (fun fmt -> function
            | None -> Format.pp_print_string fmt "None"
            | Some x -> Format.fprintf fmt "(Some %a)" pp_jpath x
          ) a1
          (fun fmt -> function
            | None -> Format.pp_print_string fmt "None"
            | Some x -> Format.fprintf fmt "(Some %S)" x
          ) a2
          pp_jaccess a3
      )
    ) x.cinner_types;
  Format.fprintf fmt "ctypes = %a" pp_jtypes x.ctypes;
  Format.fprintf fmt "@] }"

and show_jclass x = Format.asprintf "%a" pp_jclass x
