open JData;;
open ExtString;;
open ExtList;;
open IO.BigEndian;;

(* Numerical types that are not smaller than int. *)
type other_num = [
  | `Long
  | `Float
  | `Double
]


(* JVM basic type (int = short = char = byte = bool). *)
type jvm_basic_type = [
  | `Int2Bool
  | other_num
]


type java_basic_type = [
  | `Int
  | `Short
  | `Char
  | `Byte
  | `Bool
  | other_num
]


type jopcode =
  | OpNop
  | OpAConstNull
  | OpIConst of int32 | OpLConst of int64 | OpFConst of float | OpDConst of float
  | OpBIPush of int   | OpSIPush of int
  | OpLdc1 of int | OpLdc1w of int | OpLdc2w of int

  | OpLoad of jvm_basic_type * int
  | OpALoad of int

  | OpArrayLoad of [`Int | other_num]
  | OpAALoad | OpBALoad | OpCALoad | OpSALoad

  | OpStore of jvm_basic_type * int
  | OpAStore of int

  | OpArrayStore of [`Int | other_num]
  | OpAAStore | OpBAStore | OpCAStore | OpSAStore

  | OpPop  | OpPop2
  | OpDup  | OpDupX1  | OpDupX2
  | OpDup2 | OpDup2X1 | OpDup2X2
  | OpSwap

  | OpAdd of jvm_basic_type
  | OpSub of jvm_basic_type
  | OpMult of jvm_basic_type
  | OpDiv of jvm_basic_type
  | OpRem of jvm_basic_type
  | OpNeg of jvm_basic_type

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

  | OpReturn of jvm_basic_type
  | OpAReturn
  | OpReturnVoid

  | OpGetStatic of int | OpPutStatic of int
  | OpGetField of int  | OpPutField of int
  | OpInvokeVirtual of int
  | OpInvokeNonVirtual of int
  | OpInvokeStatic of int
  | OpInvokeInterface of int * int (** count *)

  | OpNew of int
  | OpNewArray of java_basic_type
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


type exc_tbl = {
  e_start : int;
  e_end : int;
  e_handler : int;
  e_catch_type : jpath option; 
}


type jcode = {
  max_stack : int;
  max_locals: int;
  code : jopcodes;
  exc_tbl : exc_tbl list;
  attributes : jattributes;
}


exception Class_structure_error of string
let error_class fmt = Printf.ksprintf (fun s -> raise (Class_structure_error s)) fmt

let get_code field =
  let rec loop = function
    | [] -> raise Not_found
    | AttrUnknown("Code",s)::xs -> s
    | _::xs -> loop xs
  in loop field.jf_attributes
