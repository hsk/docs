
(* Numerical types that are not smaller than int. *)

(* JVM basic type (int = short = char = byte = bool). *)
type jvmprim = [
  | `Int
  | `Long
  | `Float
  | `Double
]

type jprim = [
  | `Short
  | `Char
  | `Byte
  | `Bool
  | `Int
  | `Long
  | `Float
  | `Double
]

type jopcode =
  | OpNop
  | OpAConstNull
  | OpIConst of int32 | OpLConst of int64 | OpFConst of float | OpDConst of float
  | OpBIPush of int   | OpSIPush of int
  | OpLdc1 of int | OpLdc1w of int | OpLdc2w of int

  | OpLoad of jvmprim * int
  | OpALoad of int

  | OpArrayLoad of jvmprim
  | OpAALoad | OpBALoad | OpCALoad | OpSALoad

  | OpStore of jvmprim * int
  | OpAStore of int

  | OpArrayStore of jvmprim
  | OpAAStore | OpBAStore | OpCAStore | OpSAStore

  | OpPop  | OpPop2
  | OpDup  | OpDupX1  | OpDupX2
  | OpDup2 | OpDup2X1 | OpDup2X2
  | OpSwap

  | OpAdd of jvmprim | OpSub of jvmprim
  | OpMul of jvmprim | OpDiv of jvmprim
  | OpRem of jvmprim | OpNeg of jvmprim

  | OpIShl  | OpLShl
  | OpIShr  | OpLShr
  | OpIUShr | OpLUShr
  | OpIAnd  | OpLAnd
  | OpIOr   | OpLOr
  | OpIXor  | OpLXor

  | OpIInc of int * int (** index, increment *)

  | OpI2L | OpI2F | OpI2D
  | OpL2I | OpL2F | OpL2D
  | OpF2I | OpF2L | OpF2D
  | OpD2I | OpD2L | OpD2F
  | OpI2B | OpI2C | OpI2S

  | OpLCmp
  | OpFCmpL | OpFCmpG
  | OpDCmpL | OpDCmpG
  | OpIfEq of int | OpIfNe of int
  | OpIfLt of int | OpIfGe of int
  | OpIfGt of int | OpIfLe of int
  | OpICmpEq of int | OpICmpNe of int
  | OpICmpLt of int | OpICmpGe of int
  | OpICmpGt of int | OpICmpLe of int
  | OpACmpEq of int | OpACmpNe of int
  | OpGoto of int | OpJsr of int | OpRet of int

  | OpTableSwitch of int * int32 * int32 * int array
  | OpLookupSwitch of int * (int32 * int) list

  | OpReturn of jvmprim
  | OpAReturn
  | OpReturnVoid

  | OpGetStatic of int | OpPutStatic of int
  | OpGetField of int  | OpPutField of int
  | OpInvokeVirtual of int | OpInvokeSpecial of int
  | OpInvokeStatic of int  | OpInvokeInterface of int * int (** count *)

  | OpNew of int
  | OpNewArray of jprim
  | OpANewArray of int
  | OpArrayLength
  | OpThrow
  | OpCheckCast of int | OpInstanceOf of int
  | OpMonitorEnter     | OpMonitorExit
  | OpAMultiNewArray of int * int (** ClassInfo, dims *)
  | OpIfNull of int | OpIfNonNull of int
  | OpGotoW of int  | OpJsrW of int
  | OpBreakpoint                  (* should not be found *)
  (* | OpRetW of int *)
  | OpInvalid
      (* if [opcodes.(i) = OpInvalid] it means that there is an opcode
         that starts at position j, with j<i, an covers positions up
         to k, with k>=i. *)

type jopcodes = jopcode array

type try_catch = {
  e_start : int;
  e_end : int;
  e_handler : int;
  e_catch_type : JData.jpath option; 
}

type jcode = {
  max_stack : int;
  max_locals: int;
  code : jopcodes;
  try_catches : try_catch list;
  attrs : JData.jattr list;
}

exception Class_structure_error of string

let error_class fmt =
  Printf.ksprintf (fun s -> raise (Class_structure_error s)) fmt

let get_code field =
  let rec loop = function
    | [] -> raise Not_found
    | JData.AttrUnknown("Code",s)::xs -> s
    | _::xs -> loop xs
  in loop field.JData.jf_attrs
