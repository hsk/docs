open JCode

let rec pp_other_num fmt =
  function
  | `Long -> Format.pp_print_string fmt "`Long"
  | `Float -> Format.pp_print_string fmt "`Float"
  | `Double -> Format.pp_print_string fmt "`Double"
and show_other_num x = Format.asprintf "%a" pp_other_num x

let rec pp_jvm_basic_type fmt =
  function
  | `Int2Bool -> Format.pp_print_string fmt "`Int2Bool"
  | #other_num as x -> (pp_other_num fmt) x
and show_jvm_basic_type x = Format.asprintf "%a" pp_jvm_basic_type x

let rec pp_java_basic_type fmt =
  function
  | `Int -> Format.pp_print_string fmt "`Int"
  | `Short -> Format.pp_print_string fmt "`Short"
  | `Char -> Format.pp_print_string fmt "`Char"
  | `Byte -> Format.pp_print_string fmt "`Byte"
  | `Bool -> Format.pp_print_string fmt "`Bool"
  | #other_num as x -> (pp_other_num fmt) x
and show_java_basic_type x = Format.asprintf "%a" pp_java_basic_type x

let rec pp_jopcode fmt =
  function
  | OpNop -> Format.pp_print_string fmt "JCode.OpNop"
  | OpAConstNull -> Format.pp_print_string fmt "JCode.OpAConstNull"
  | OpIConst a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpIConst@ %ldl@])" a0;
  | OpLConst a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpLConst@ %LdL@])" a0;
  | OpFConst a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpFConst@ %F@])" a0;
  | OpDConst a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpDConst@ %F@])" a0;
  | OpBIPush a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpBIPush@ %d@])" a0;
  | OpSIPush a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpSIPush@ %d@])" a0;
  | OpLdc1 a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpLdc1@ %d@])" a0;
  | OpLdc1w a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpLdc1w@ %d@])" a0;
  | OpLdc2w a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpLdc2w@ %d@])" a0;
  | OpLoad (a0,a1) -> Format.fprintf fmt "@[<hov2>JCode.OpLoad (@,%a,@ %d@])" pp_jvm_basic_type a0 a1;
  | OpALoad a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpALoad@ %d@])" a0;
  | OpArrayLoad a0 ->
      Format.fprintf fmt "(@[<hov2>JCode.OpArrayLoad@ ";
      (function
        | `Int -> Format.pp_print_string fmt "`Int"
        | #other_num as x -> pp_other_num fmt x
      ) a0;
      Format.fprintf fmt "@])"
  | OpAALoad -> Format.pp_print_string fmt "JCode.OpAALoad"
  | OpBALoad -> Format.pp_print_string fmt "JCode.OpBALoad"
  | OpCALoad -> Format.pp_print_string fmt "JCode.OpCALoad"
  | OpSALoad -> Format.pp_print_string fmt "JCode.OpSALoad"
  | OpStore (a0,a1) -> Format.fprintf fmt "@[<hov2>JCode.OpStore (@,%a,@ %d@])" pp_jvm_basic_type a0 a1;
  | OpAStore a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpAStore@ %d@])" a0;
  | OpArrayStore a0 ->
      Format.fprintf fmt "(@[<hov2>JCode.OpArrayStore@ ";
      (function
        | `Int -> Format.pp_print_string fmt "`Int"
        | #other_num as x -> pp_other_num fmt x
      ) a0;
      Format.fprintf fmt "@])"
  | OpAAStore -> Format.pp_print_string fmt "JCode.OpAAStore"
  | OpBAStore -> Format.pp_print_string fmt "JCode.OpBAStore"
  | OpCAStore -> Format.pp_print_string fmt "JCode.OpCAStore"
  | OpSAStore -> Format.pp_print_string fmt "JCode.OpSAStore"
  | OpPop -> Format.pp_print_string fmt "JCode.OpPop"
  | OpPop2 -> Format.pp_print_string fmt "JCode.OpPop2"
  | OpDup -> Format.pp_print_string fmt "JCode.OpDup"
  | OpDupX1 -> Format.pp_print_string fmt "JCode.OpDupX1"
  | OpDupX2 -> Format.pp_print_string fmt "JCode.OpDupX2"
  | OpDup2 -> Format.pp_print_string fmt "JCode.OpDup2"
  | OpDup2X1 -> Format.pp_print_string fmt "JCode.OpDup2X1"
  | OpDup2X2 -> Format.pp_print_string fmt "JCode.OpDup2X2"
  | OpSwap -> Format.pp_print_string fmt "JCode.OpSwap"
  | OpAdd a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpAdd@ %a@])" pp_jvm_basic_type a0;
  | OpSub a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpSub@ %a@])" pp_jvm_basic_type a0;
  | OpMult a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpMult@ %a@])" pp_jvm_basic_type a0;
  | OpDiv a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpDiv@ %a@])" pp_jvm_basic_type a0;
  | OpRem a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpRem@ %a@])" pp_jvm_basic_type a0;
  | OpNeg a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpNeg@ %a@])" pp_jvm_basic_type a0;
  | OpIShl -> Format.pp_print_string fmt "JCode.OpIShl"
  | OpLShl -> Format.pp_print_string fmt "JCode.OpLShl"
  | OpIShr -> Format.pp_print_string fmt "JCode.OpIShr"
  | OpLShr -> Format.pp_print_string fmt "JCode.OpLShr"
  | OpIUShr -> Format.pp_print_string fmt "JCode.OpIUShr"
  | OpLUShr -> Format.pp_print_string fmt "JCode.OpLUShr"
  | OpIAnd -> Format.pp_print_string fmt "JCode.OpIAnd"
  | OpLAnd -> Format.pp_print_string fmt "JCode.OpLAnd"
  | OpIOr -> Format.pp_print_string fmt "JCode.OpIOr"
  | OpLOr -> Format.pp_print_string fmt "JCode.OpLOr"
  | OpIXor -> Format.pp_print_string fmt "JCode.OpIXor"
  | OpLXor -> Format.pp_print_string fmt "JCode.OpLXor"
  | OpIInc (a0,a1) -> Format.fprintf fmt "@[<hov2>JCode.OpIInc (@,%d,@ %d@])" a0 a1;
  | OpI2L -> Format.pp_print_string fmt "JCode.OpI2L"
  | OpI2F -> Format.pp_print_string fmt "JCode.OpI2F"
  | OpI2D -> Format.pp_print_string fmt "JCode.OpI2D"
  | OpL2I -> Format.pp_print_string fmt "JCode.OpL2I"
  | OpL2F -> Format.pp_print_string fmt "JCode.OpL2F"
  | OpL2D -> Format.pp_print_string fmt "JCode.OpL2D"
  | OpF2I -> Format.pp_print_string fmt "JCode.OpF2I"
  | OpF2L -> Format.pp_print_string fmt "JCode.OpF2L"
  | OpF2D -> Format.pp_print_string fmt "JCode.OpF2D"
  | OpD2I -> Format.pp_print_string fmt "JCode.OpD2I"
  | OpD2L -> Format.pp_print_string fmt "JCode.OpD2L"
  | OpD2F -> Format.pp_print_string fmt "JCode.OpD2F"
  | OpI2B -> Format.pp_print_string fmt "JCode.OpI2B"
  | OpI2C -> Format.pp_print_string fmt "JCode.OpI2C"
  | OpI2S -> Format.pp_print_string fmt "JCode.OpI2S"
  | OpLCmp -> Format.pp_print_string fmt "JCode.OpLCmp"
  | OpFCmpL -> Format.pp_print_string fmt "JCode.OpFCmpL"
  | OpFCmpG -> Format.pp_print_string fmt "JCode.OpFCmpG"
  | OpDCmpL -> Format.pp_print_string fmt "JCode.OpDCmpL"
  | OpDCmpG -> Format.pp_print_string fmt "JCode.OpDCmpG"
  | OpIfEq a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpIfEq@ %d@])" a0;
  | OpIfNe a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpIfNe@ %d@])" a0;
  | OpIfLt a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpIfLt@ %d@])" a0;
  | OpIfGe a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpIfGe@ %d@])" a0;
  | OpIfGt a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpIfGt@ %d@])" a0;
  | OpIfLe a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpIfLe@ %d@])" a0;
  | OpICmpEq a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpICmpEq@ %d@])" a0;
  | OpICmpNe a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpICmpNe@ %d@])" a0;
  | OpICmpLt a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpICmpLt@ %d@])" a0;
  | OpICmpGe a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpICmpGe@ %d@])" a0;
  | OpICmpGt a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpICmpGt@ %d@])" a0;
  | OpICmpLe a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpICmpLe@ %d@])" a0;
  | OpACmpEq a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpACmpEq@ %d@])" a0;
  | OpACmpNe a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpACmpNe@ %d@])" a0;
  | OpGoto a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpGoto@ %d@])" a0;
  | OpJsr a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpJsr@ %d@])" a0;
  | OpRet a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpRet@ %d@])" a0;
  | OpTableSwitch (a0,a1,a2,a3) ->
      Format.fprintf fmt "@[<hov2>JCode.OpTableSwitch (@,%d,@ " a0;
      Format.fprintf fmt "%ldl,@ " a1;
      Format.fprintf fmt "%ldl,@ " a2;
      Format.fprintf fmt "[|@[<hov>";
      Array.iter (Format.fprintf fmt "%d;@ ") a3;
      Format.fprintf fmt "@]|]";
      Format.fprintf fmt "@])"
  | OpLookupSwitch (a0,a1) ->
      Format.fprintf fmt "@[<hov2>JCode.OpLookupSwitch (@,%d,@ " a0;
      Format.fprintf fmt "[@[<hov>";
      List.iter (fun (a0, a1) ->
        Format.fprintf fmt "(@[<hov>%ldl,@ %d@]);@ " a0 a1;
      ) a1;
      Format.fprintf fmt "@]]";
      Format.fprintf fmt "@])"
  | OpReturn a0 ->
      Format.fprintf fmt "(@[<hov2>JCode.OpReturn@ %a@])" pp_jvm_basic_type a0;
  | OpAReturn -> Format.pp_print_string fmt "JCode.OpAReturn"
  | OpReturnVoid -> Format.pp_print_string fmt "JCode.OpReturnVoid"
  | OpGetStatic a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpGetStatic@ %d@])" a0;
  | OpPutStatic a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpPutStatic@ %d@])" a0;
  | OpGetField a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpGetField@ %d@])" a0;
  | OpPutField a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpPutField@ %d@])" a0;
  | OpInvokeVirtual a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpInvokeVirtual@ %d@])" a0;
  | OpInvokeNonVirtual a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpInvokeNonVirtual@ %d@])" a0;
  | OpInvokeStatic a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpInvokeStatic@ %d@])" a0;
  | OpInvokeInterface (a0,a1) -> Format.fprintf fmt "@[<hov2>JCode.OpInvokeInterface (@,%d,@ %d@])" a0 a1;
  | OpNew a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpNew@ %d@])" a0;
  | OpNewArray a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpNewArray@ %a@])" pp_java_basic_type a0;
  | OpANewArray a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpANewArray@ %d@])" a0;
  | OpArrayLength -> Format.pp_print_string fmt "JCode.OpArrayLength"
  | OpThrow -> Format.pp_print_string fmt "JCode.OpThrow"
  | OpCheckCast a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpCheckCast@ %d@])" a0;
  | OpInstanceOf a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpInstanceOf@ %d@])" a0;
  | OpMonitorEnter -> Format.pp_print_string fmt "JCode.OpMonitorEnter"
  | OpMonitorExit -> Format.pp_print_string fmt "JCode.OpMonitorExit"
  | OpAMultiNewArray (a0,a1) -> Format.fprintf fmt "@[<hov2>JCode.OpAMultiNewArray (@,%d,@ %d@])" a0 a1;
  | OpIfNull a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpIfNull@ %d@])" a0;
  | OpIfNonNull a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpIfNonNull@ %d@])" a0;
  | OpGotoW a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpGotoW@ %d@])" a0;
  | OpJsrW a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpJsrW@ %d@])" a0;
  | OpBreakpoint -> Format.pp_print_string fmt "JCode.OpBreakpoint"
  | OpInvalid -> Format.pp_print_string fmt "JCode.OpInvalid"
and show_jopcode x = Format.asprintf "%a" pp_jopcode x

let rec pp_jopcodes fmt x =
  Format.fprintf fmt "[|@[<hov>";
  Array.iter (Format.fprintf fmt "%a;@ " pp_jopcode) x;
  Format.fprintf fmt "@]|]"
and show_jopcodes x = Format.asprintf "%a" pp_jopcodes x

let rec pp_exc_tbl fmt x =
  Format.fprintf fmt "{ @[<hov>";
  Format.fprintf fmt "JCode.e_start = %d;@ " x.e_start;
  Format.fprintf fmt "e_end = %d;@ " x.e_end;
  Format.fprintf fmt "e_handler = %d;@ " x.e_handler;
  Format.fprintf fmt "e_catch_type = %a"
    (fun fmt -> function
        | None -> Format.pp_print_string fmt "None"
        | Some x -> Format.fprintf fmt "(Some %a)" JDataPP.pp_jpath x
    ) x.e_catch_type;
  Format.fprintf fmt "@] }"
and show_exc_tbl x = Format.asprintf "%a" pp_exc_tbl x

let rec pp_jcode fmt x =
  Format.fprintf fmt "{ @[<hov>";
  Format.fprintf fmt "JCode.max_stack = %d;@ " x.max_stack;
  Format.fprintf fmt "max_locals = %d;@ " x.max_locals;
  Format.fprintf fmt "code = %a;@ " pp_jopcodes x.code;
  Format.fprintf fmt "exc_tbl = [@[<hov>%a@]];@ "
    (fun fmt ->
      List.iter (Format.fprintf fmt "%a;@ " pp_exc_tbl)
    ) x.exc_tbl;
  Format.fprintf fmt "attributes = %a" JDataPP.pp_jattributes x.attributes;
  Format.fprintf fmt "@] }"
and show_jcode x = Format.asprintf "%a" pp_jcode x

