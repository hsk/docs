open IO;;
open IO.BigEndian;;
open JData;;
open JCode;;

exception Opcode_length_error of int * JCode.jopcode
let error_len length op = raise (Opcode_length_error (length, op))

let encode_jvm_basic_type = function
  | `Int -> 0
  | `Long -> 1
  | `Float -> 2
  | `Double -> 3

let padding ch count =
  flush ch;
  for i = 1 + (count () - 1) mod 4 to 3 do
    write_byte ch 0
  done

let encode_instruction ch count length op =
  let wb opcode =
    if length <> 1 then error_len length op;
    write_byte ch opcode
  in
  let jb opcode jvm_basic_type =
    if length <> 1 then error_len length op;
    let opcode = opcode + encode_jvm_basic_type jvm_basic_type in
    write_byte ch opcode
  in
  let i16 opcode i =
    if length <> 3 then error_len length op;
    write_byte ch opcode;
    write_i16 ch i
  in
  let ui16 opcode i =
    if length <> 3 then error_len length op;
    write_byte ch opcode;
    write_ui16 ch i
  in
  let li opcode i =
    if length = 2 && i <= 0xFF then (
      write_byte ch opcode;
      write_byte ch i
    ) else if length = 4 then (
      write_byte ch 196;
      write_byte ch opcode;
      write_ui16 ch i
    ) else (
      error_len length op
    )
  in

  match op with
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
    | OpAReturn      -> wb 176 | OpReturnVoid  -> wb 177
    | OpArrayLength  -> wb 190 | OpThrow       -> wb 191
    | OpMonitorEnter -> wb 194 | OpMonitorExit -> wb 195
    | OpBreakpoint   -> wb 202

    | OpArrayLoad  k -> jb 46 k
    | OpArrayStore k -> jb 79 k

    | OpAdd    i -> jb  96 i
    | OpSub    i -> jb 100 i
    | OpMult   i -> jb 104 i
    | OpDiv    i -> jb 108 i
    | OpRem    i -> jb 112 i
    | OpNeg    i -> jb 116 i
    | OpReturn i -> jb 172 i

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

    | OpLdc1w            i -> ui16 19  i | OpLdc2w      i -> ui16 20  i
    | OpNew              i -> ui16 187 i | OpANewArray  i -> ui16 189 i 
    | OpCheckCast        i -> ui16 192 i | OpInstanceOf i -> ui16 193 i
    | OpGetStatic        i -> ui16 178 i | OpPutStatic  i -> ui16 179 i
    | OpGetField         i -> ui16 180 i | OpPutField   i -> ui16 181 i
    | OpInvokeVirtual    i -> ui16 182 i
    | OpInvokeNonVirtual i -> ui16 183 i
    | OpInvokeStatic     i -> ui16 184 i

    | OpIConst n ->
        if not (-1l <= n && n <= 5l)
        then error_class "iconst %ld Arguments of iconst should be between -1l and 5l (inclusive)" n;
        if length <> 1 then error_len length op;
        write_byte ch (3 + Int32.to_int n)
    | OpLConst n ->
        if not (0L=n || n=1L)
        then error_class "lconst Arguments of lconst should be 0L or 1L";
        if length <> 1 then error_len length op;
        write_byte ch (9 + Int64.to_int n)
    | OpFConst n ->
        if not (0.=n || n=1. || n=2.)
        then error_class "fconst Arguments of fconst should be 0., 1. or 2.";
        if length <> 1 then error_len length op;
        write_byte ch (11 + int_of_float n)
    | OpDConst n ->
        if not (0.=n || n=1.)
        then error_class "dconst Arguments of dconst should be 0. or 1.";
        if length <> 1 then error_len length op;
        write_byte ch (14 + int_of_float n)
    | OpBIPush n ->
        if length <> 2 then error_len length op;
        write_byte ch 16;
        write_byte ch n
    | OpLdc1 index ->
        if length <> 2 then error_len length op;
        write_byte ch 18;
        write_byte ch index
    | OpIInc (index, incr) ->
        if length = 3 && index <= 0xFF && - 0x80 <= incr && incr <= 0x7F
        then (
          write_byte ch 132; write_byte ch index; write_byte ch incr
        ) else
        if length = 6 then (
          write_byte ch 196;
          write_byte ch 132; write_ui16 ch index; write_i16 ch incr
        ) else
        error_len length op
    | OpRet pc ->
        if length = 2 && pc <= 0xFF
        then (
          write_byte ch 169; write_byte ch pc
        ) else
        if length = 4 then (
          write_byte ch 196;
          write_byte ch 169; write_ui16 ch pc
        ) else
        error_len length op
    | OpTableSwitch (def, low, high, tbl) ->
        flush ch;
        let c = count () in
        let padding_size = (4 - ((c + 1) mod 4)) mod 4 in
        if length <> 13 + padding_size + 4 * (Array.length tbl)
        then error_len length op;
        write_byte ch 170;
        padding ch count;
        write_i32 ch def;
        write_real_i32 ch low;
        write_real_i32 ch high;
        Array.iter (write_i32 ch) tbl
    | OpLookupSwitch (def, tbl) ->
        flush ch;
        let padding_size = (4 - (count () + 1) mod 4) mod 4 in
        if length <> 9 + padding_size + 8 * (List.length tbl)
        then error_len length op;
        write_byte ch 171;
        padding ch count;
        write_i32 ch def;
        write_i32 ch (List.length tbl);
        List.iter begin function v, j ->
          write_real_i32 ch v;
          write_i32 ch j
        end tbl
    | OpInvokeInterface (index, nargs) ->
        if length <> 5 then error_len length op;
        write_byte ch 185;
        write_ui16 ch index;
        write_byte ch nargs;
        write_byte ch 0
    | OpNewArray t ->
        if length <> 2 then error_len length op;
        write_byte ch 188;
        begin match t with
          | `Bool   -> write_byte ch 4
          | `Char   -> write_byte ch 5
          | `Float  -> write_byte ch 6
          | `Double -> write_byte ch 7
          | `Byte   -> write_byte ch 8
          | `Short  -> write_byte ch 9
          | `Int    -> write_byte ch 10
          | `Long   -> write_byte ch 11
        end
    | OpAMultiNewArray (c, dims) ->
        if length <> 4 then error_len length op;
        write_byte ch 197;
        write_ui16 ch c;
        write_byte ch dims
    | OpGotoW i ->
        if length <> 5 then error_len length op;
        write_byte ch 200;
        write_i32 ch i
    | OpJsrW i ->
        if length <> 5 then error_len length op;
        write_byte ch 201;
        write_i32 ch i
    | OpInvalid -> ()
    | OpLoad (jbt, i) ->
      if length = 1 && i <= 3 then
        write_byte ch (26 + (encode_jvm_basic_type jbt) * 4 + i)
      else
        li (21 + encode_jvm_basic_type jbt) i
    | OpStore (jbt, i) ->
      if length = 1 && i <= 3 then
        write_byte ch (59 + (encode_jvm_basic_type jbt) * 4 + i)
      else
        li (54 + encode_jvm_basic_type jbt) i
    | OpALoad  i -> if length = 1 && i <= 3 then write_byte ch (42 + i) else li 25 i
    | OpAStore i -> if length = 1 && i <= 3 then write_byte ch (75 + i) else li 58 i
        
let encode_codes ch code =
  let ch, count = pos_out ch in
  Array.iteri (fun i opcode ->
    let length =
      let j = ref (i + 1) in
      while !j < Array.length code && code.(!j) = OpInvalid do
        incr j
      done;
      !j - i
    in
    if not (opcode = OpInvalid || count () = i)
    then error_class "unparsing Badly alligned low level bytecode";
    encode_instruction ch count length opcode
  ) code;
  if not (count () = Array.length code)
  then error_class "unparsing Badly alligned low level bytecode"

let encode_code ctx { max_stack; max_locals; code; exc_tbl; attributes } =
  let ch = ctx.JWriter.ch in
  write_i16 ch max_stack;
  write_i16 ch max_locals;
  write_i32 ch (Array.length code);
  encode_codes ch code;
  write_ui16 ch (List.length exc_tbl);
  List.iter (fun {e_start;e_end;e_handler;e_catch_type} ->
    write_ui16 ch e_start;
    write_ui16 ch e_end;
    write_ui16 ch e_handler;
    match e_catch_type with
    | None -> write_ui16 ch 0
    | Some e -> write_ui16 ch (JWriter.const ctx (ConstClass e))
  ) exc_tbl;
  write_ui16 ch (List.length attributes);
  List.iter (fun attrib ->
    JWriter.encode_attribute ctx attrib
  ) attributes

