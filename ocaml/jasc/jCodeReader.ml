open ExtList;;
open IO.BigEndian;;
open JData;;
open JCode;;
open JReader;;

let jvmprim place = function
    | 0 -> `Int
    | 1 -> `Long
    | 2 -> `Float
    | 3 -> `Double
    | n -> error_class "Illegal type of %s: %d" place n

let read_unsigned ch wide =
  if wide then read_ui16 ch else IO.read_byte ch

let read_signed ch wide =
  if wide then read_i16 ch else IO.read_signed_byte ch

let parse_opcode op ch wide =
  match op with
  | 0 -> OpNop
  (* ---- push ----------------------------------- *)
  | 1 -> OpAConstNull
  | 2 -> OpIConst Int32.minus_one
  | 3 | 4 | 5 | 6 | 7 | 8 -> OpIConst (Int32.of_int (op - 3))
  | 9  -> OpLConst Int64.zero
  | 10 -> OpLConst Int64.one
  | 11 | 12 | 13 -> OpFConst (float_of_int (op - 11))
  | 14 -> OpDConst 0. | 15 -> OpDConst 1.
  | 16 -> OpBIPush (IO.read_signed_byte ch)
  | 17 -> OpSIPush (read_i16 ch)
  | 18 -> OpLdc1 (IO.read_byte ch)
  | 19 -> OpLdc1w (read_ui16 ch)
  | 20 -> OpLdc2w (read_ui16 ch)
  (* ---- load ----------------------------------- *)
  | 21 | 22 | 23 | 24 -> OpLoad (jvmprim "load" (op - 21),read_unsigned ch wide)
  | 25                -> OpALoad (read_unsigned ch wide)
  | 26 | 27 | 28 | 29 -> OpLoad (`Int,op - 26)
  | 30 | 31 | 32 | 33 -> OpLoad (`Long,op - 30)
  | 34 | 35 | 36 | 37 -> OpLoad (`Float,op - 34)
  | 38 | 39 | 40 | 41 -> OpLoad (`Double,op - 38)
  | 42 | 43 | 44 | 45 -> OpALoad (op - 42)
  (* ---- array load ---------------------------- *)
  | 46 | 47 | 48 | 49 -> OpArrayLoad (jvmprim "arrayload" (op - 46))
  | 50 -> OpAALoad | 51 -> OpBALoad | 52 -> OpCALoad | 53 -> OpSALoad
  (* ---- store ----------------------------------- *)
  | 54 | 55 | 56 | 57 -> OpStore (jvmprim "store" (op - 54),read_unsigned ch wide)
  | 58                -> OpAStore (read_unsigned ch wide)
  | 59 | 60 | 61 | 62 -> OpStore (`Int , op - 59)
  | 63 | 64 | 65 | 66 -> OpStore (`Long , op - 63)
  | 67 | 68 | 69 | 70 -> OpStore (`Float , op - 67)
  | 71 | 72 | 73 | 74 -> OpStore (`Double , op - 71)
  | 75 | 76 | 77 | 78 -> OpAStore (op - 75)
  (* ---- array store ---------------------------- *)
  | 79 | 80 | 81 | 82 -> OpArrayStore (jvmprim "arraystore" (op - 79))
  | 83 -> OpAAStore | 84 -> OpBAStore | 85 -> OpCAStore | 86 -> OpSAStore
  (* ---- stack ---------------------------------- *)
  | 87 -> OpPop  | 88 -> OpPop2
  | 89 -> OpDup  | 90 -> OpDupX1  | 91 -> OpDupX2
  | 92 -> OpDup2 | 93 -> OpDup2X1 | 94 -> OpDup2X2
  | 95 -> OpSwap
  (* ---- arithmetics ---------------------------- *)
  | 96  | 97  | 98  | 99  -> OpAdd (jvmprim "add" (op - 96))
  | 100 | 101 | 102 | 103 -> OpSub (jvmprim "sub" (op - 100))
  | 104 | 105 | 106 | 107 -> OpMul (jvmprim "mul" (op - 104))
  | 108 | 109 | 110 | 111 -> OpDiv (jvmprim "div" (op - 108))
  | 112 | 113 | 114 | 115 -> OpRem (jvmprim "rem" (op - 112))
  | 116 | 117 | 118 | 119 -> OpNeg (jvmprim "neg" (op - 116))
  (* ---- logicals ------------------------------- *)
  | 120 -> OpIShl  | 121 -> OpLShl
  | 122 -> OpIShr  | 123 -> OpLShr 
  | 124 -> OpIUShr | 125 -> OpLUShr
  | 126 -> OpIAnd  | 127 -> OpLAnd
  | 128 -> OpIOr   | 129 -> OpLOr
  | 130 -> OpIXor  | 131 -> OpLXor
  (* ---- incr ----------------------------------- *)
  | 132 ->
      let idx = read_unsigned ch wide in
      let c = read_signed ch wide in
      OpIInc (idx,c)
  (* ---- conversions ---------------------------- *)
  | 133 -> OpI2L | 134 -> OpI2F | 135 -> OpI2D
  | 136 -> OpL2I | 137 -> OpL2F | 138 -> OpL2D
  | 139 -> OpF2I | 140 -> OpF2L | 141 -> OpF2D
  | 142 -> OpD2I | 143 -> OpD2L | 144 -> OpD2F
  | 145 -> OpI2B | 146 -> OpI2C | 147 -> OpI2S
  (* ---- jumps ---------------------------------- *)
  | 148 -> OpLCmp
  | 149 -> OpFCmpL | 150 -> OpFCmpG
  | 151 -> OpDCmpL | 152 -> OpDCmpG

  | 153 -> OpIfEq (read_i16 ch) | 154 -> OpIfNe (read_i16 ch)
  | 155 -> OpIfLt (read_i16 ch) | 156 -> OpIfGe (read_i16 ch)
  | 157 -> OpIfGt (read_i16 ch) | 158 -> OpIfLe (read_i16 ch)

  | 159 -> OpICmpEq (read_i16 ch) | 160 -> OpICmpNe (read_i16 ch)
  | 161 -> OpICmpLt (read_i16 ch) | 162 -> OpICmpGe (read_i16 ch)
  | 163 -> OpICmpGt (read_i16 ch) | 164 -> OpICmpLe (read_i16 ch)
  | 165 -> OpACmpEq (read_i16 ch) | 166 -> OpACmpNe (read_i16 ch)

  | 167 -> OpGoto (read_i16 ch)
  | 168 -> OpJsr (read_i16 ch)
  | 169 -> OpRet (read_unsigned ch wide)
  | 170 ->
      let def = read_i32 ch in
      let low = read_real_i32 ch in
      let high = read_real_i32 ch in
      let tbl = Array.init (Int32.to_int (Int32.sub high low) + 1) (fun _ -> read_i32 ch) in
      OpTableSwitch (def,low,high,tbl)
  | 171 ->
      let def = read_i32 ch in
      let npairs = read_i32 ch in
      let tbl = List.init npairs (fun _ ->
        let v = read_real_i32 ch in
        let j = read_i32 ch in
        v , j
      ) in
      OpLookupSwitch (def,tbl)
  (* ---- returns --------------------------------- *)
  | 172 | 173 | 174 | 175 -> OpReturn (jvmprim "return" (op - 172))
  | 176 -> OpAReturn
  | 177 -> OpReturnVoid
  (* ---- OO ------------------------------------- *)
  | 178 -> OpGetStatic (read_ui16 ch) | 179 -> OpPutStatic (read_ui16 ch)
  | 180 -> OpGetField  (read_ui16 ch) | 181 -> OpPutField  (read_ui16 ch)

  | 182 -> OpInvokeVirtual (read_ui16 ch)
  | 183 -> OpInvokeSpecial (read_ui16 ch)
  | 184 -> OpInvokeStatic (read_ui16 ch)
  | 185 ->
      let index = read_ui16 ch in
      let nargs = IO.read_byte ch in
      let _ = IO.read_byte ch in
      OpInvokeInterface (index, nargs)
  (* ---- others --------------------------------- *)
  | 187 -> OpNew (read_ui16 ch)
  | 188 ->
      OpNewArray begin match IO.read_byte ch with
        | 4 -> `Bool
        | 5 -> `Char
        | 6 -> `Float
        | 7 -> `Double
        | 8 -> `Byte
        | 9 -> `Short
        | 10 -> `Int
        | 11 -> `Long
        | n -> error_class "Illegal type of newarray: %d" n
      end
  | 189 -> OpANewArray (read_ui16 ch)
  | 190 -> OpArrayLength

  | 191 -> OpThrow
  | 192 -> OpCheckCast (read_ui16 ch) | 193 -> OpInstanceOf (read_ui16 ch)
  | 194 -> OpMonitorEnter             | 195 -> OpMonitorExit

  | 197 ->
      let c = read_ui16 ch in
      let dims = IO.read_byte ch in
      OpAMultiNewArray (c,dims)

  | 198 -> OpIfNull (read_i16 ch) | 199 -> OpIfNonNull (read_i16 ch)
  | 200 -> OpGotoW (read_i32 ch)  | 201 -> OpJsrW (read_i32 ch)
  | 202 -> OpBreakpoint

  | _ -> error_class "Illegal opcode: %d" op

let parse_code consts code =
  let ch = IO.input_string code in
  let max_stack = read_i16 ch in
  let max_locals = read_i16 ch in
  let len = read_i32 ch in
  let ch , pos = IO.pos_in ch in
  let codes = Array.create len OpInvalid in
  while pos() < len do
    let p = pos() in
    let op = IO.read_byte ch in
    if op = 196 then
      codes.(p) <- parse_opcode (IO.read_byte ch) ch true else
    let offsetmod4 = (p + 1) mod 4 in
    if (op = 170 || op = 171) && offsetmod4 > 0
    then ignore(IO.really_nread ch (4 - offsetmod4));
    codes.(p) <- parse_opcode op ch false
  done;
  let len = read_ui16 ch in
  let try_catches = List.init len begin fun _ ->
    {
      e_start = read_ui16 ch;
      e_end = read_ui16 ch;
      e_handler = read_ui16 ch;
      e_catch_type =
        begin match read_ui16 ch with
        | 0 -> None
        | ct ->
          debug "ct %d\n" ct;
          match get_constant consts ct with
          | ConstClass c -> Some c
          | _ -> error_class "Illegal class index (does not refer to a constant class)"
        end;
    }
  end in
  let attrib_count = read_ui16 ch in
  let attribs = parse_attrs ~on_special:(fun _ _ aname alen do_default ->
    match aname with
    | _ -> do_default()
  ) consts ch attrib_count in {
    max_stack = max_stack;
    max_locals = max_locals;
    code = codes;
    try_catches = try_catches;
    attrs = attribs;
  }
