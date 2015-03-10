type other_num = [ `Long | `Float | `Double]
let rec pp_other_num fmt =
  function
  | `Long -> Format.pp_print_string fmt "`Long"
  | `Float -> Format.pp_print_string fmt "`Float"
  | `Double -> Format.pp_print_string fmt "`Double"
and show_other_num x = Format.asprintf "%a" pp_other_num x
type jvm_basic_type = [ `Int2Bool | other_num]
let rec pp_jvm_basic_type fmt =
  function
  | `Int2Bool -> Format.pp_print_string fmt "`Int2Bool"
  | #other_num as x -> (pp_other_num fmt) x
and show_jvm_basic_type x = Format.asprintf "%a" pp_jvm_basic_type x
type java_basic_type = [ `Int | `Short | `Char | `Byte | `Bool | other_num]

let rec pp_java_basic_type fmt =
  function
  | `Int -> Format.pp_print_string fmt "`Int"
  | `Short -> Format.pp_print_string fmt "`Short"
  | `Char -> Format.pp_print_string fmt "`Char"
  | `Byte -> Format.pp_print_string fmt "`Byte"
  | `Bool -> Format.pp_print_string fmt "`Bool"
  | #other_num as x -> (pp_other_num fmt) x
and show_java_basic_type x = Format.asprintf "%a" pp_java_basic_type x
type jopcode =
  | OpNop
  | OpAConstNull
  | OpIConst of int32
  | OpLConst of int64
  | OpFConst of float
  | OpDConst of float
  | OpBIPush of int
  | OpSIPush of int
  | OpLdc1 of int
  | OpLdc1w of int
  | OpLdc2w of int
  | OpLoad of jvm_basic_type* int
  | OpALoad of int
  | OpArrayLoad of [ `Int | other_num]
  | OpAALoad
  | OpBALoad
  | OpCALoad
  | OpSALoad
  | OpStore of jvm_basic_type* int
  | OpAStore of int
  | OpArrayStore of [ `Int | other_num]
  | OpAAStore
  | OpBAStore
  | OpCAStore
  | OpSAStore
  | OpPop
  | OpPop2
  | OpDup
  | OpDupX1
  | OpDupX2
  | OpDup2
  | OpDup2X1
  | OpDup2X2
  | OpSwap
  | OpAdd of jvm_basic_type
  | OpSub of jvm_basic_type
  | OpMult of jvm_basic_type
  | OpDiv of jvm_basic_type
  | OpRem of jvm_basic_type
  | OpNeg of jvm_basic_type
  | OpIShl
  | OpLShl
  | OpIShr
  | OpLShr
  | OpIUShr
  | OpLUShr
  | OpIAnd
  | OpLAnd
  | OpIOr
  | OpLOr
  | OpIXor
  | OpLXor
  | OpIInc of int* int
  | OpI2L
  | OpI2F
  | OpI2D
  | OpL2I
  | OpL2F
  | OpL2D
  | OpF2I
  | OpF2L
  | OpF2D
  | OpD2I
  | OpD2L
  | OpD2F
  | OpI2B
  | OpI2C
  | OpI2S
  | OpLCmp
  | OpFCmpL
  | OpFCmpG
  | OpDCmpL
  | OpDCmpG
  | OpIfEq of int
  | OpIfNe of int
  | OpIfLt of int
  | OpIfGe of int
  | OpIfGt of int
  | OpIfLe of int
  | OpICmpEq of int
  | OpICmpNe of int
  | OpICmpLt of int
  | OpICmpGe of int
  | OpICmpGt of int
  | OpICmpLe of int
  | OpACmpEq of int
  | OpACmpNe of int
  | OpGoto of int
  | OpJsr of int
  | OpRet of int
  | OpTableSwitch of int* int32* int32* int array
  | OpLookupSwitch of int* (int32* int) list
  | OpReturn of jvm_basic_type
  | OpAReturn
  | OpReturnVoid
  | OpGetStatic of int
  | OpPutStatic of int
  | OpGetField of int
  | OpPutField of int
  | OpInvokeVirtual of int
  | OpInvokeNonVirtual of int
  | OpInvokeStatic of int
  | OpInvokeInterface of int* int
  | OpNew of int
  | OpNewArray of java_basic_type
  | OpANewArray of int
  | OpArrayLength
  | OpThrow
  | OpCheckCast of int
  | OpInstanceOf of int
  | OpMonitorEnter
  | OpMonitorExit
  | OpAMultiNewArray of int* int
  | OpIfNull of int
  | OpIfNonNull of int
  | OpGotoW of int
  | OpJsrW of int
  | OpBreakpoint
  | OpInvalid
let rec pp_jopcode fmt =
  function
  | OpNop  -> Format.pp_print_string fmt "JCode.OpNop"
  | OpAConstNull  -> Format.pp_print_string fmt "JCode.OpAConstNull"
  | OpIConst a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpIConst@ ";
       (Format.fprintf fmt "%ldl") a0;
       Format.fprintf fmt "@])")
  | OpLConst a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpLConst@ ";
       (Format.fprintf fmt "%LdL") a0;
       Format.fprintf fmt "@])")
  | OpFConst a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpFConst@ ";
       (Format.fprintf fmt "%F") a0;
       Format.fprintf fmt "@])")
  | OpDConst a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpDConst@ ";
       (Format.fprintf fmt "%F") a0;
       Format.fprintf fmt "@])")
  | OpBIPush a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpBIPush@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpSIPush a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpSIPush@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpLdc1 a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpLdc1@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpLdc1w a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpLdc1w@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpLdc2w a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpLdc2w@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpLoad (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>JCode.OpLoad (@,";
       ((pp_jvm_basic_type fmt) a0;
        Format.fprintf fmt ",@ ";
        (Format.fprintf fmt "%d") a1);
       Format.fprintf fmt "@])")
  | OpALoad a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpALoad@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpArrayLoad a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpArrayLoad@ ";
       ((function
         | `Int -> Format.pp_print_string fmt "`Int"
         | #other_num as x -> (pp_other_num fmt) x)) a0;
       Format.fprintf fmt "@])")
  | OpAALoad  -> Format.pp_print_string fmt "JCode.OpAALoad"
  | OpBALoad  -> Format.pp_print_string fmt "JCode.OpBALoad"
  | OpCALoad  -> Format.pp_print_string fmt "JCode.OpCALoad"
  | OpSALoad  -> Format.pp_print_string fmt "JCode.OpSALoad"
  | OpStore (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>JCode.OpStore (@,";
       ((pp_jvm_basic_type fmt) a0;
        Format.fprintf fmt ",@ ";
        (Format.fprintf fmt "%d") a1);
       Format.fprintf fmt "@])")
  | OpAStore a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpAStore@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpArrayStore a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpArrayStore@ ";
       ((function
         | `Int -> Format.pp_print_string fmt "`Int"
         | #other_num as x -> (pp_other_num fmt) x)) a0;
       Format.fprintf fmt "@])")
  | OpAAStore  -> Format.pp_print_string fmt "JCode.OpAAStore"
  | OpBAStore  -> Format.pp_print_string fmt "JCode.OpBAStore"
  | OpCAStore  -> Format.pp_print_string fmt "JCode.OpCAStore"
  | OpSAStore  -> Format.pp_print_string fmt "JCode.OpSAStore"
  | OpPop  -> Format.pp_print_string fmt "JCode.OpPop"
  | OpPop2  -> Format.pp_print_string fmt "JCode.OpPop2"
  | OpDup  -> Format.pp_print_string fmt "JCode.OpDup"
  | OpDupX1  -> Format.pp_print_string fmt "JCode.OpDupX1"
  | OpDupX2  -> Format.pp_print_string fmt "JCode.OpDupX2"
  | OpDup2  -> Format.pp_print_string fmt "JCode.OpDup2"
  | OpDup2X1  -> Format.pp_print_string fmt "JCode.OpDup2X1"
  | OpDup2X2  -> Format.pp_print_string fmt "JCode.OpDup2X2"
  | OpSwap  -> Format.pp_print_string fmt "JCode.OpSwap"
  | OpAdd a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpAdd@ ";
       (pp_jvm_basic_type fmt) a0;
       Format.fprintf fmt "@])")
  | OpSub a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpSub@ ";
       (pp_jvm_basic_type fmt) a0;
       Format.fprintf fmt "@])")
  | OpMult a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpMult@ ";
       (pp_jvm_basic_type fmt) a0;
       Format.fprintf fmt "@])")
  | OpDiv a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpDiv@ ";
       (pp_jvm_basic_type fmt) a0;
       Format.fprintf fmt "@])")
  | OpRem a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpRem@ ";
       (pp_jvm_basic_type fmt) a0;
       Format.fprintf fmt "@])")
  | OpNeg a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpNeg@ ";
       (pp_jvm_basic_type fmt) a0;
       Format.fprintf fmt "@])")
  | OpIShl  -> Format.pp_print_string fmt "JCode.OpIShl"
  | OpLShl  -> Format.pp_print_string fmt "JCode.OpLShl"
  | OpIShr  -> Format.pp_print_string fmt "JCode.OpIShr"
  | OpLShr  -> Format.pp_print_string fmt "JCode.OpLShr"
  | OpIUShr  -> Format.pp_print_string fmt "JCode.OpIUShr"
  | OpLUShr  -> Format.pp_print_string fmt "JCode.OpLUShr"
  | OpIAnd  -> Format.pp_print_string fmt "JCode.OpIAnd"
  | OpLAnd  -> Format.pp_print_string fmt "JCode.OpLAnd"
  | OpIOr  -> Format.pp_print_string fmt "JCode.OpIOr"
  | OpLOr  -> Format.pp_print_string fmt "JCode.OpLOr"
  | OpIXor  -> Format.pp_print_string fmt "JCode.OpIXor"
  | OpLXor  -> Format.pp_print_string fmt "JCode.OpLXor"
  | OpIInc (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>JCode.OpIInc (@,";
       ((Format.fprintf fmt "%d") a0;
        Format.fprintf fmt ",@ ";
        (Format.fprintf fmt "%d") a1);
       Format.fprintf fmt "@])")
  | OpI2L  -> Format.pp_print_string fmt "JCode.OpI2L"
  | OpI2F  -> Format.pp_print_string fmt "JCode.OpI2F"
  | OpI2D  -> Format.pp_print_string fmt "JCode.OpI2D"
  | OpL2I  -> Format.pp_print_string fmt "JCode.OpL2I"
  | OpL2F  -> Format.pp_print_string fmt "JCode.OpL2F"
  | OpL2D  -> Format.pp_print_string fmt "JCode.OpL2D"
  | OpF2I  -> Format.pp_print_string fmt "JCode.OpF2I"
  | OpF2L  -> Format.pp_print_string fmt "JCode.OpF2L"
  | OpF2D  -> Format.pp_print_string fmt "JCode.OpF2D"
  | OpD2I  -> Format.pp_print_string fmt "JCode.OpD2I"
  | OpD2L  -> Format.pp_print_string fmt "JCode.OpD2L"
  | OpD2F  -> Format.pp_print_string fmt "JCode.OpD2F"
  | OpI2B  -> Format.pp_print_string fmt "JCode.OpI2B"
  | OpI2C  -> Format.pp_print_string fmt "JCode.OpI2C"
  | OpI2S  -> Format.pp_print_string fmt "JCode.OpI2S"
  | OpLCmp  -> Format.pp_print_string fmt "JCode.OpLCmp"
  | OpFCmpL  -> Format.pp_print_string fmt "JCode.OpFCmpL"
  | OpFCmpG  -> Format.pp_print_string fmt "JCode.OpFCmpG"
  | OpDCmpL  -> Format.pp_print_string fmt "JCode.OpDCmpL"
  | OpDCmpG  -> Format.pp_print_string fmt "JCode.OpDCmpG"
  | OpIfEq a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpIfEq@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpIfNe a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpIfNe@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpIfLt a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpIfLt@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpIfGe a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpIfGe@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpIfGt a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpIfGt@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpIfLe a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpIfLe@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpICmpEq a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpICmpEq@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpICmpNe a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpICmpNe@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpICmpLt a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpICmpLt@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpICmpGe a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpICmpGe@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpICmpGt a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpICmpGt@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpICmpLe a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpICmpLe@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpACmpEq a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpACmpEq@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpACmpNe a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpACmpNe@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpGoto a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpGoto@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpJsr a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpJsr@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpRet a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpRet@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpTableSwitch (a0,a1,a2,a3) ->
      (Format.fprintf fmt "@[<hov2>JCode.OpTableSwitch (@,";
       ((((Format.fprintf fmt "%d") a0;
          Format.fprintf fmt ",@ ";
          (Format.fprintf fmt "%ldl") a1);
         Format.fprintf fmt ",@ ";
         (Format.fprintf fmt "%ldl") a2);
        Format.fprintf fmt ",@ ";
        ((fun x  ->
            Format.fprintf fmt "[|@[<hov>";
            ignore
              (Array.fold_left
                 (fun sep  ->
                    fun x  ->
                      if sep then Format.fprintf fmt ";@ ";
                      (Format.fprintf fmt "%d") x;
                      true) false x);
            Format.fprintf fmt "@]|]")) a3);
       Format.fprintf fmt "@])")
  | OpLookupSwitch (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>JCode.OpLookupSwitch (@,";
       ((Format.fprintf fmt "%d") a0;
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
                          ((Format.fprintf fmt "%ldl") a0;
                           Format.fprintf fmt ",@ ";
                           (Format.fprintf fmt "%d") a1);
                          Format.fprintf fmt "@])")) x;
                      true) false x);
            Format.fprintf fmt "@]]")) a1);
       Format.fprintf fmt "@])")
  | OpReturn a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpReturn@ ";
       (pp_jvm_basic_type fmt) a0;
       Format.fprintf fmt "@])")
  | OpAReturn  -> Format.pp_print_string fmt "JCode.OpAReturn"
  | OpReturnVoid  -> Format.pp_print_string fmt "JCode.OpReturnVoid"
  | OpGetStatic a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpGetStatic@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpPutStatic a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpPutStatic@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpGetField a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpGetField@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpPutField a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpPutField@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpInvokeVirtual a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpInvokeVirtual@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpInvokeNonVirtual a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpInvokeNonVirtual@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpInvokeStatic a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpInvokeStatic@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpInvokeInterface (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>JCode.OpInvokeInterface (@,";
       ((Format.fprintf fmt "%d") a0;
        Format.fprintf fmt ",@ ";
        (Format.fprintf fmt "%d") a1);
       Format.fprintf fmt "@])")
  | OpNew a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpNew@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpNewArray a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpNewArray@ ";
       (pp_java_basic_type fmt) a0;
       Format.fprintf fmt "@])")
  | OpANewArray a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpANewArray@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpArrayLength  -> Format.pp_print_string fmt "JCode.OpArrayLength"
  | OpThrow  -> Format.pp_print_string fmt "JCode.OpThrow"
  | OpCheckCast a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpCheckCast@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpInstanceOf a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpInstanceOf@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpMonitorEnter  -> Format.pp_print_string fmt "JCode.OpMonitorEnter"
  | OpMonitorExit  -> Format.pp_print_string fmt "JCode.OpMonitorExit"
  | OpAMultiNewArray (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>JCode.OpAMultiNewArray (@,";
       ((Format.fprintf fmt "%d") a0;
        Format.fprintf fmt ",@ ";
        (Format.fprintf fmt "%d") a1);
       Format.fprintf fmt "@])")
  | OpIfNull a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpIfNull@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpIfNonNull a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpIfNonNull@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpGotoW a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpGotoW@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpJsrW a0 ->
      (Format.fprintf fmt "(@[<hov2>JCode.OpJsrW@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpBreakpoint  -> Format.pp_print_string fmt "JCode.OpBreakpoint"
  | OpInvalid  -> Format.pp_print_string fmt "JCode.OpInvalid"
and show_jopcode x = Format.asprintf "%a" pp_jopcode x
type jopcodes = jopcode array
let rec pp_jopcodes fmt x =
  Format.fprintf fmt "[|@[<hov>";
  ignore
    (Array.fold_left
       (fun sep  ->
          fun x  ->
            if sep then Format.fprintf fmt ";@ "; (pp_jopcode fmt) x; true)
       false x);
  Format.fprintf fmt "@]|]"
and show_jopcodes x = Format.asprintf "%a" pp_jopcodes x
type exc_tbl =
  {
  e_start: int;
  e_end: int;
  e_handler: int;
  e_catch_type: JData.jpath option;}
let rec pp_exc_tbl fmt x =
  Format.fprintf fmt "{ @[<hov>";
  ((((Format.pp_print_string fmt "JCode.e_start = ";
      (Format.fprintf fmt "%d") x.e_start);
     Format.fprintf fmt ";@ ";
     Format.pp_print_string fmt "e_end = ";
     (Format.fprintf fmt "%d") x.e_end);
    Format.fprintf fmt ";@ ";
    Format.pp_print_string fmt "e_handler = ";
    (Format.fprintf fmt "%d") x.e_handler);
   Format.fprintf fmt ";@ ";
   Format.pp_print_string fmt "e_catch_type = ";
   ((function
     | None  -> Format.pp_print_string fmt "None"
     | Some x ->
         (Format.pp_print_string fmt "(Some ";
          (JData.pp_jpath fmt) x;
          Format.pp_print_string fmt ")"))) x.e_catch_type);
  Format.fprintf fmt "@] }"
and show_exc_tbl x = Format.asprintf "%a" pp_exc_tbl x
type jcode =
  {
  max_stack: int;
  max_locals: int;
  code: jopcodes;
  exc_tbl: exc_tbl list;
  attributes: JData.jattributes;}
let rec pp_jcode fmt x =
  Format.fprintf fmt "{ @[<hov>";
  (((((Format.pp_print_string fmt "JCode.max_stack = ";
       (Format.fprintf fmt "%d") x.max_stack);
      Format.fprintf fmt ";@ ";
      Format.pp_print_string fmt "max_locals = ";
      (Format.fprintf fmt "%d") x.max_locals);
     Format.fprintf fmt ";@ ";
     Format.pp_print_string fmt "code = ";
     (pp_jopcodes fmt) x.code);
    Format.fprintf fmt ";@ ";
    Format.pp_print_string fmt "exc_tbl = ";
    ((fun x  ->
        Format.fprintf fmt "[@[<hov>";
        ignore
          (List.fold_left
             (fun sep  ->
                fun x  ->
                  if sep then Format.fprintf fmt ";@ ";
                  (pp_exc_tbl fmt) x;
                  true) false x);
        Format.fprintf fmt "@]]")) x.exc_tbl);
   Format.fprintf fmt ";@ ";
   Format.pp_print_string fmt "attributes = ";
   (JData.pp_jattributes fmt) x.attributes);
  Format.fprintf fmt "@] }"
and show_jcode x = Format.asprintf "%a" pp_jcode x
exception Class_structure_error of string
let error_class fmt =
  Printf.ksprintf (fun s  -> raise (Class_structure_error s)) fmt
let get_code field =
  let rec loop =
    function
    | [] -> raise Not_found
    | (JData.AttrUnknown ("Code",s))::xs -> s
    | _::xs -> loop xs in
  loop field.JData.jf_attributes
