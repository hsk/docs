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

let rec pp_jtype_arg fmt =
  function
  | TExtends (a0) -> Format.fprintf fmt "@[<hov2>JData.TExtends (@,%a@])" pp_jty a0;
  | TSuper (a0) -> Format.fprintf fmt "@[<hov2>JData.TSuper (@,%a@])" pp_jty a0;
  | TType (a0) -> Format.fprintf fmt "@[<hov2>JData.TType (@,%a@])" pp_jty a0;
  | TAny -> Format.pp_print_string fmt "JData.TAny"
and show_jtype_arg x = Format.asprintf "%a" pp_jtype_arg x
and pp_jty fmt =
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
            (fun fmt -> List.iter(Format.fprintf fmt "%a;@ " pp_jtype_arg))
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
                  (fun fmt -> List.iter(Format.fprintf fmt "%a;@ " pp_jtype_arg))
              ) a1
          ) a1;
          Format.fprintf fmt "@]]"
        ) a1
  | TArray (a0,a1) ->
      Format.fprintf fmt "@[<hov2>JData.TArray (@,%a,@ %a@])"
        pp_jty a0
        (fun fmt -> function
            | None -> Format.pp_print_string fmt "None"
            | Some x -> Format.fprintf fmt "(Some %d)" x
        ) a1
  | TMethod a0 ->
      Format.fprintf fmt "(@[<hov2>JData.TMethod@ %a@])"
        pp_jmty a0;
  | TTypeParameter a0 ->
      Format.fprintf fmt "(@[<hov2>JData.TTypeParameter@ %S@])" a0;
and show_jty x = Format.asprintf "%a" pp_jty x

and pp_jmty fmt (a0,a1) =
  Format.fprintf fmt "(@[<hov>";
  Format.fprintf fmt "[@[<hov>";
  List.iter(Format.fprintf fmt "%a;@ " pp_jty) a0;
  Format.fprintf fmt "@]],@ ";
  (function
     | None -> Format.pp_print_string fmt "None"
     | Some x -> Format.fprintf fmt "(Some %a)" pp_jty x
  ) a1;
  Format.fprintf fmt "@])"
and show_jmty x = Format.asprintf "%a" pp_jmty x

let rec pp_jtys fmt x =
  Format.fprintf fmt "[@[<hov>";
  List.iter(Format.fprintf fmt "%a;@ " pp_jty) x;
  Format.fprintf fmt "@]]"
and show_jtys x = Format.asprintf "%a" pp_jtys x

let rec pp_ref_type fmt =
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
and show_ref_type x = Format.asprintf "%a" pp_ref_type x

let rec pp_bootstrap_method fmt = Format.fprintf fmt "%d"
and show_bootstrap_method x = Format.asprintf "%a" pp_bootstrap_method x

let rec pp_jconst fmt =
  function
  | ConstClass a0 ->
      Format.fprintf fmt "(@[<hov2>JData.ConstClass@ %a@])" pp_jpath a0;
  | ConstField (a0,a1,a2) ->
      Format.fprintf fmt "(@[<hov2>JData.ConstField@ ";
      Format.fprintf fmt "(@[<hov>%a,@ %S,@ %a@])" pp_jpath a0 a1 pp_jty a2;
      Format.fprintf fmt "@])"
  | ConstMethod (a0,a1,a2) ->
      Format.fprintf fmt "(@[<hov2>JData.ConstMethod@ ";
      Format.fprintf fmt "(@[<hov>%a,@ %S,@ %a@])"
        pp_jpath a0 a1 pp_jmty a2;
      Format.fprintf fmt "@])"
  | ConstInterfaceMethod (a0,a1,a2) ->
      Format.fprintf fmt "(@[<hov2>JData.ConstInterfaceMethod@ ";
      Format.fprintf fmt "(@[<hov>%a,@ %S,@ %a@])" pp_jpath a0 a1 pp_jmty a2;
      Format.fprintf fmt "@])"
  | ConstString a0 -> Format.fprintf fmt "(@[<hov2>JData.ConstString@ %S@])" a0;
  | ConstInt a0 -> Format.fprintf fmt "(@[<hov2>JData.ConstInt@ %ldl@])" a0;
  | ConstFloat a0 -> Format.fprintf fmt "(@[<hov2>JData.ConstFloat@ %F@])" a0;
  | ConstLong a0 -> Format.fprintf fmt "(@[<hov2>JData.ConstLong@ %LdL@])" a0;
  | ConstDouble a0 -> Format.fprintf fmt "(@[<hov2>JData.ConstDouble@ %F@])" a0;
  | ConstNameAndType (a0,a1) ->
      Format.fprintf fmt "@[<hov2>JData.ConstNameAndType (@,%S,@ %a@])"
        a0 pp_jty a1;
  | ConstUtf8 a0 ->
      Format.fprintf fmt "(@[<hov2>JData.ConstUtf8@ %S@])" a0;
  | ConstMethodHandle (a0,a1) ->
      Format.fprintf fmt "(@[<hov2>JData.ConstMethodHandle@ ";
      Format.fprintf fmt "(@[<hov>%a,@ %a@])" pp_ref_type a0 pp_jconst a1;
      Format.fprintf fmt "@])"
  | ConstMethodType a0 ->
      Format.fprintf fmt "(@[<hov2>JData.ConstMethodType@ %a@])" pp_jmty a0;
  | ConstInvokeDynamic (a0,a1,a2) ->
      Format.fprintf fmt "(@[<hov2>JData.ConstInvokeDynamic@ ";
      Format.fprintf fmt "(@[<hov>%a,@ %S,@ %a@])" pp_bootstrap_method a0 a1 pp_jty a2;
      Format.fprintf fmt "@])"
  | ConstUnusable -> Format.pp_print_string fmt "JData.ConstUnusable"
and show_jconst x = Format.asprintf "%a" pp_jconst x

let pp_jcode fmt code = Format.fprintf fmt "()"
let show_jcode code = "()"

let rec pp_jaccess fmt =
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
and show_jaccess x = Format.asprintf "%a" pp_jaccess x

let rec pp_jaccesses fmt x =
  Format.fprintf fmt "[@[<hov>%a@]]"
    (fun fmt -> List.iter (Format.fprintf fmt "%a;@ " pp_jaccess)) x
and show_jaccesses x = Format.asprintf "%a" pp_jaccesses x

let rec pp_jtypes fmt x =
  Format.fprintf fmt "[@[<hov>%a@]]"
    (fun fmt ->
      List.iter (fun (a0,a1,a2) ->
        Format.fprintf fmt "(@[<hov>%S,@ %a,@ %a@]);@ "
          a0
          (fun fmt -> function
              | None -> Format.pp_print_string fmt "None"
              | Some x -> Format.fprintf fmt "(Some %a)" pp_jty x
          ) a1
          (fun fmt ->
            Format.fprintf fmt "[@[<hov>%a@]]"
              (fun fmt -> List.iter (Format.fprintf fmt "%a;@ " pp_jty))
          ) a2;
      )
    ) x
and show_jtypes x = Format.asprintf "%a" pp_jtypes x

let rec pp_jannotation fmt x =
  Format.fprintf fmt "{ @[<hov>";
  Format.fprintf fmt "JData.ann_type = %a;@ " pp_jty x.ann_type;
  Format.fprintf fmt "ann_elements = [@[<hov>%a@]]@ "
    (fun fmt ->
      List.iter (fun (a0, a1) -> Format.fprintf fmt "(@[<hov>%S,@ %a@]);@ " a0 pp_jannotation_value a1)
    ) x.ann_elements;
  Format.fprintf fmt "@] }"
and show_jannotation x = Format.asprintf "%a" pp_jannotation x
and pp_jannotation_value fmt =
  function
  | ValConst (a0,a1) ->
      Format.fprintf fmt "@[<hov2>JData.ValConst (@,%d,@ %a@])" a0 pp_jconst a1
  | ValEnum (a0,a1) ->
      Format.fprintf fmt "@[<hov2>JData.ValEnum (@,%a,@ %S@])" pp_jty a0 a1
  | ValClass a0 ->
      Format.fprintf fmt "(@[<hov2>JData.ValClass@ %a@])" pp_jty a0;
  | ValAnnotation a0 ->
      Format.fprintf fmt "(@[<hov2>JData.ValAnnotation@ %a@])" pp_jannotation a0
  | ValArray a0 ->
      Format.fprintf fmt "(@[<hov2>JData.ValArray@ ";
      Format.fprintf fmt "[@[<hov>%a@]]"
        (fun fmt -> List.iter (Format.fprintf fmt "%a;@ " pp_jannotation_value)) a0;
      Format.fprintf fmt "@])"
and show_jannotation_value x = Format.asprintf "%a" pp_jannotation_value x

let rec pp_jattr fmt = function
  | AttrDeprecated -> Format.pp_print_string fmt "JData.AttrDeprecated"
  | AttrVisibleAnnotations a0 ->
      Format.fprintf fmt "(@[<hov2>JData.AttrVisibleAnnotations@ %a@])"
        (fun fmt ->
          Format.fprintf fmt "[@[<hov>%a@]]"
            (fun fmt -> List.iter (Format.fprintf fmt "%a;@ " pp_jannotation))
        ) a0;
  | AttrInvisibleAnnotations a0 ->
      Format.fprintf fmt "(@[<hov2>JData.AttrInvisibleAnnotations@ ";
      Format.fprintf fmt "[@[<hov>%a@]]"
        (fun fmt -> List.iter (Format.fprintf fmt "%a;@ " pp_jannotation)) a0;
      Format.fprintf fmt "@])"
  | AttrUnknown (a0,a1) ->
      Format.fprintf fmt "@[<hov2>JData.AttrUnknown (@,%S,@ %S@])" a0 a1
and show_jattr x = Format.asprintf "%a" pp_jattr x

let rec pp_jattrs fmt x =
  Format.fprintf fmt "[@[<hov>";
  List.iter (Format.fprintf fmt "%a;@ " pp_jattr) x;
  Format.fprintf fmt "@]]"
and show_jattrs x = Format.asprintf "%a" pp_jattrs x

let rec pp_jfield_kind fmt = function
  | JKField -> Format.pp_print_string fmt "JData.JKField"
  | JKMethod -> Format.pp_print_string fmt "JData.JKMethod"
and show_jfield_kind x = Format.asprintf "%a" pp_jfield_kind x

let rec pp_jfield fmt x =
  Format.fprintf fmt "{ @[<hov>";
  Format.fprintf fmt "JData.jf_name = %S;@ " x.jf_name;
  Format.fprintf fmt "jf_kind = %a;@ " pp_jfield_kind x.jf_kind;
  Format.fprintf fmt "jf_vmty = %a;@ " pp_jty x.jf_vmty;
  Format.fprintf fmt "jf_ty = %a;@ " pp_jty x.jf_ty;
  Format.fprintf fmt "jf_throws = [@[<hov>%a@]];@ "
    (fun fmt -> List.iter (Format.fprintf fmt "%a;@ " pp_jty)) x.jf_throws;
  Format.fprintf fmt "jf_types = %a;@ " pp_jtypes x.jf_types;
  Format.fprintf fmt "jf_accs = %a;@ " pp_jaccesses x.jf_accs;
  Format.fprintf fmt "jf_attrs = [@[<hov>%a@]];@ "
    (fun fmt -> List.iter (Format.fprintf fmt "%a;@ " pp_jattr)) x.jf_attrs;
  Format.fprintf fmt "jf_const = %a;@ "
    (fun fmt -> function
        | None -> Format.pp_print_string fmt "None"
        | Some x -> Format.fprintf fmt "(Some %a)" pp_jconst x
    ) x.jf_const;
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

let rec pp_jclass fmt x =
  Format.fprintf fmt "{ @[<hov>";
  Format.fprintf fmt "JData.cversion = %d,%d;@ " (fst x.cversion) (snd x.cversion);
  Format.fprintf fmt "consts = [|@[<hov>%a@]|];@ "
    (fun fmt ->
      Array.iteri (fun i x -> Format.fprintf fmt "(* %3d *) %a;@ " i pp_jconst x)
    ) x.consts;
  Format.fprintf fmt "cpath = %a;@ " pp_jpath x.cpath;
  Format.fprintf fmt "csuper = %a;@ " pp_jty x.csuper;
  Format.fprintf fmt "caccs = %a;@ " pp_jaccesses x.caccs;
  Format.fprintf fmt "cinterfaces = [@[<hov>%a@]];@ "
    (fun fmt -> List.iter (Format.fprintf fmt "%a;@ " pp_jty)) x.cinterfaces;
  Format.fprintf fmt "cfields = [@[<hov>%a@]];@ "
    (fun fmt -> List.iter (Format.fprintf fmt "%a;@ " pp_jfield)) x.cfields;
  Format.fprintf fmt "cmethods = [@[<hov>%a@]];@ "
    (fun fmt -> List.iter (Format.fprintf fmt "%a;@ " pp_jfield)) x.cmethods;
  Format.fprintf fmt "cattrs = [@[<hov>%a@]];@ "
    (fun fmt -> List.iter (Format.fprintf fmt "%a;@ " pp_jattr)) x.cattrs;
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
          pp_jaccesses a3
      )
    ) x.cinner_types;
  Format.fprintf fmt "ctypes = %a" pp_jtypes x.ctypes;
  Format.fprintf fmt "@] }"

and show_jclass x = Format.asprintf "%a" pp_jclass x
