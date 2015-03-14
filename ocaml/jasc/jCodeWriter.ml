open IO;;
open IO.BigEndian;;
open JData;;
open JCode;;

exception Opcode_length_error of int * int * JCode.jopcode

let encode_jvm_prim = function
  | `Int    -> 0
  | `Long   -> 1
  | `Float  -> 2
  | `Double -> 3

let encode_prim = function
  | `Bool   -> 4
  | `Char   -> 5
  | `Float  -> 6
  | `Double -> 7
  | `Byte   -> 8
  | `Short  -> 9
  | `Int    -> 10
  | `Long   -> 11

let rec padding ch count =
  if (count ()) mod 4 > 0 then (
    write_byte ch 0;
    padding ch count
  )

let encode_instruction ch count length op =
  let check_length len =
    if length <> len then raise (Opcode_length_error (len, length, op));
  in
  let wb opcode =
    check_length 1;
    write_byte ch opcode
  in
  let jb opcode jvm_prim =
    check_length 1;
    let opcode = opcode + encode_jvm_prim jvm_prim in
    write_byte ch opcode
  in
  let i16 opcode i =
    check_length 3;
    write_byte ch opcode;
    write_i16 ch i
  in
  let ui16 opcode i =
    check_length 3;
    write_byte ch opcode;
    write_ui16 ch i
  in
  let li16 opcode i =
    check_length 4;
    write_byte ch 196;
    write_byte ch opcode;
    write_ui16 ch i
  in
  let lb opcode i =
    check_length 2;
    if i >= 0xFF then error_class "Argument error %d > 255 " i;
    write_byte ch opcode;
    write_byte ch i
  in

  match op with
    (* length 0 *)
    | OpInvalid -> ()
    (* length 1 *)
    | OpNop     -> wb 0   | OpAConstNull -> wb 1
    | OpAALoad  -> wb 50  | OpBALoad  -> wb 51 | OpCALoad  -> wb 52 | OpSALoad  -> wb 53
    | OpAAStore -> wb 83  | OpBAStore -> wb 84 | OpCAStore -> wb 85 | OpSAStore -> wb 86
    | OpPop     -> wb 87  | OpPop2    -> wb 88
    | OpDup     -> wb 89  | OpDupX1   -> wb 90 | OpDupX2   -> wb 91
    | OpDup2    -> wb 92  | OpDup2X1  -> wb 93 | OpDup2X2  -> wb 94
    | OpSwap    -> wb 95
    | OpIShl    -> wb 120 | OpLShl    -> wb 121
    | OpIShr    -> wb 122 | OpLShr    -> wb 123
    | OpIUShr   -> wb 124 | OpLUShr   -> wb 125
    | OpIAnd    -> wb 126 | OpLAnd    -> wb 127
    | OpIOr     -> wb 128 | OpLOr     -> wb 129
    | OpIXor    -> wb 130 | OpLXor    -> wb 131
    | OpI2L     -> wb 133 | OpI2F     -> wb 134 | OpI2D -> wb 135
    | OpL2I     -> wb 136 | OpL2F     -> wb 137 | OpL2D -> wb 138
    | OpF2I     -> wb 139 | OpF2L     -> wb 140 | OpF2D -> wb 141
    | OpD2I     -> wb 142 | OpD2L     -> wb 143 | OpD2F -> wb 144
    | OpI2B     -> wb 145 | OpI2C     -> wb 146 | OpI2S -> wb 147
    | OpLCmp    -> wb 148
    | OpFCmpL   -> wb 149 | OpFCmpG   -> wb 150
    | OpDCmpL   -> wb 151 | OpDCmpG   -> wb 152
    | OpAReturn      -> wb 176   | OpReturnVoid   -> wb 177
    | OpArrayLength  -> wb 190   | OpThrow        -> wb 191
    | OpMonitorEnter -> wb 194   | OpMonitorExit  -> wb 195
    | OpBreakpoint   -> wb 202

    | OpArrayLoad  k -> jb  46 k | OpArrayStore k -> jb 79 k

    | OpAdd        i -> jb  96 i | OpSub        i -> jb 100 i
    | OpMul        i -> jb 104 i | OpDiv        i -> jb 108 i
    | OpRem        i -> jb 112 i | OpNeg        i -> jb 116 i
    | OpReturn     i -> jb 172 i

    | OpIConst n ->
        if not (-1l <= n && n <= 5l)
        then error_class "iconst %ld Arguments of iconst should be between -1l and 5l (inclusive)" n;
        check_length 1;
        write_byte ch (3 + Int32.to_int n)
    | OpLConst n ->
        if not (0L=n || n=1L)
        then error_class "lconst Arguments of lconst should be 0L or 1L";
        check_length 1;
        write_byte ch (9 + Int64.to_int n)
    | OpFConst n ->
        if not (0.=n || n=1. || n=2.)
        then error_class "fconst Arguments of fconst should be 0., 1. or 2.";
        check_length 1;
        write_byte ch (11 + int_of_float n)
    | OpDConst n ->
        if not (0.=n || n=1.)
        then error_class "dconst Arguments of dconst should be 0. or 1.";
        check_length 1;
        write_byte ch (14 + int_of_float n)
    | OpLoad1 (jbt, i) ->
        if not (0 <= i && i <= 3)
        then error_class "load_%d Arguments of load should be between 0 and 3 (inclusive)" i;
        check_length 1;
        write_byte ch (26 + (encode_jvm_prim jbt) * 4 + i)
    | OpStore1 (jbt, i) ->
        if not (0 <= i && i <= 3)
        then error_class "store_%d Arguments of store should be between 0 and 3 (inclusive)" i;
        check_length 1;
        write_byte ch (59 + (encode_jvm_prim jbt) * 4 + i)
    | OpALoad1  (i) ->
        if not (0 <= i && i <= 3)
        then error_class "store_%d Arguments of store should be between 0 and 3 (inclusive)" i;
        check_length 1;
        write_byte ch (42 + i)
    | OpAStore1 (i) ->
        if not (0 <= i && i <= 3)
        then error_class "astore_%d Arguments of astore should be between 0 and 3 (inclusive)" i;
        check_length 1;
        write_byte ch (75 + i)
    
    (* length 2 *)
    | OpBIPush n -> lb 16 n
    | OpLdc1 index -> lb 18 index
    | OpLoad  (jbt, i, false) -> lb (21 + encode_jvm_prim jbt) i
    | OpStore (jbt, i, false) -> lb (54 + encode_jvm_prim jbt) i
    | OpALoad  (i, false)     -> lb 25 i
    | OpAStore (i, false)     -> lb 58 i
    | OpRet    (i, false)     -> lb 169 i
    | OpNewArray t            -> lb 188 (encode_prim t)

    (* length 3 *)
    | OpSIPush i -> i16  17 i
    | OpIfEq   i -> i16 153 i | OpIfNe   i -> i16 154 i
    | OpIfLt   i -> i16 155 i | OpIfGe   i -> i16 156 i
    | OpIfGt   i -> i16 157 i | OpIfLe   i -> i16 158 i
    | OpICmpEq i -> i16 159 i | OpICmpNe i -> i16 160 i
    | OpICmpLt i -> i16 161 i | OpICmpGe i -> i16 162 i
    | OpICmpGt i -> i16 163 i | OpICmpLe i -> i16 164 i
    | OpACmpEq i -> i16 165 i | OpACmpNe i -> i16 166 i
    | OpGoto   i -> i16 167 i | OpJsr    i -> i16 168 i
    | OpIfNull i -> i16 198 i | OpIfNonNull i -> i16 199 i

    | OpLdc1w         i -> ui16 19  i | OpLdc2w      i -> ui16 20  i
    | OpNew           i -> ui16 187 i | OpANewArray  i -> ui16 189 i 
    | OpCheckCast     i -> ui16 192 i | OpInstanceOf i -> ui16 193 i
    | OpGetStatic     i -> ui16 178 i | OpPutStatic  i -> ui16 179 i
    | OpGetField      i -> ui16 180 i | OpPutField   i -> ui16 181 i
    | OpInvokeVirtual i -> ui16 182 i
    | OpInvokeSpecial i -> ui16 183 i
    | OpInvokeStatic  i -> ui16 184 i

    | OpIInc (index, incr, false) ->
        check_length 3;
        if not (index <= 0xFF && - 0x80 <= incr && incr <= 0x7F)
        then error_class "iinc arguments error %d %d" index incr;
        write_byte ch 132; write_byte ch index; write_byte ch incr

    (* length 4 *)
    | OpALoad  (i, true)     -> li16 25 i
    | OpAStore (i, true)     -> li16 58 i
    | OpRet    (i, true)     -> li16 169 i
    | OpLoad  (jbt, i, true) -> li16 (21 + encode_jvm_prim jbt) i
    | OpStore (jbt, i, true) -> li16 (54 + encode_jvm_prim jbt) i

    | OpAMultiNewArray (c, dims) ->
        check_length 4;
        write_byte ch 197;
        write_ui16 ch c;
        write_byte ch dims

    (* length 5 *)
    | OpInvokeInterface (index, nargs) ->
        check_length 5;
        write_byte ch 185;
        write_ui16 ch index;
        write_byte ch nargs;
        write_byte ch 0
    | OpGotoW i ->
        check_length 5;
        write_byte ch 200;
        write_i32 ch i
    | OpJsrW i ->
        check_length 5;
        write_byte ch 201;
        write_i32 ch i
    
    (* length 6 *)
    | OpIInc (index, incr, true) ->
        check_length 6;
        write_byte ch 196;
        write_byte ch 132; write_ui16 ch index; write_i16 ch incr
    (* length n *)
    | OpTableSwitch (def, low, high, tbl) ->
        let c = count () in
        write_byte ch 170;
        padding ch count;
        write_i32 ch def;
        write_real_i32 ch low;
        write_real_i32 ch high;
        Array.iter (write_i32 ch) tbl;
        check_length ((count()) - c)
    | OpLookupSwitch (def, tbl) ->
        let c = count () in
        write_byte ch 171;
        padding ch count;
        write_i32 ch def;
        write_i32 ch (List.length tbl);
        List.iter begin function v, j ->
          write_real_i32 ch v;
          write_i32 ch j
        end tbl;
        check_length ((count()) - c)

let encode_codes ch code =
  let ch, count = pos_out ch in
  Array.iteri (fun i op ->
    if not (op = OpInvalid || count () = i)
    then error_class "unparsing Badly alligned low level bytecode";
    let rec get_len p =
      if not (p < Array.length code && code.(p) = OpInvalid) then p - i
      else get_len (p + 1)
    in
    encode_instruction ch count (get_len (i + 1)) op
  ) code;
  if not (count () = Array.length code)
  then error_class "unparsing Badly alligned low level bytecode"

let encode_code ctx { max_stack; max_locals; code; try_catches; attrs } =
  let ch = ctx.JWriter.ch in
  write_i16 ch max_stack;
  write_i16 ch max_locals;
  write_i32 ch (Array.length code);
  encode_codes ch code;
  write_ui16 ch (List.length try_catches);
  List.iter (fun {e_start;e_end;e_handler;e_catch_type} ->
    write_ui16 ch e_start;
    write_ui16 ch e_end;
    write_ui16 ch e_handler;
    match e_catch_type with
    | None -> write_ui16 ch 0
    | Some e -> write_ui16 ch (JWriter.const ctx (ConstClass e))
  ) try_catches;

  write_ui16 ch (List.length attrs);
  List.iter (fun attr -> JWriter.encode_attr ctx attr) attrs

