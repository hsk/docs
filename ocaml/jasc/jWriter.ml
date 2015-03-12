(*
 *  This file is part of JavaLib
 *  Copyright (c)2004-2012 Nicolas Cannasse and Caue Waneck
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)
open IO;;
open IO.BigEndian;;
open JData;;

let debug = debug0

exception Writer_error_message of string

type context = {
  cpool : string output;
  mutable ccount : int;
  mutable ch : string output;
  mutable consts : (jconst, int) PMap.t;
}

let encode_utf8 s = s

let get_ref_type i =
  match i with
  | RGetField         -> 1
  | RGetStatic        -> 2
  | RPutField         -> 3
  | RPutStatic        -> 4
  | RInvokeVirtual    -> 5
  | RInvokeStatic     -> 6
  | RInvokeSpecial    -> 7
  | RNewInvokeSpecial -> 8
  | RInvokeInterface  -> 9

let error msg = raise (Writer_error_message msg)
let error fmt = Printf.ksprintf (fun s -> raise (Writer_error_message s)) fmt

let encode_path (pack, name) =
  String.concat "/" (pack @ [name])

let rec encode_ty s =
  let encode_type_parameter = function
    | TAny  -> "*"
    | TExtends ty -> "+" ^ (encode_ty ty)
    | TSuper ty -> "-" ^ (encode_ty ty)
    | TType ty -> encode_ty ty
  in
  let encode_params params =
    match params with
    | [] -> ""
    | _ -> Printf.sprintf "<%s>" (String.concat "" (List.map (encode_type_parameter) params))
  in
  match s with
  | TByte   -> "B"
  | TChar   -> "C"
  | TDouble -> "D"
  | TFloat  -> "F"
  | TInt    -> "I"
  | TLong   -> "J"
  | TShort  -> "S"
  | TBool   -> "Z"
  | TArray (s, Some size) -> Printf.sprintf "[%d%s" size (encode_ty s)
  | TArray (s, None)      -> Printf.sprintf "[%s" (encode_ty s)
  | TObject((pack, name), params) ->
    Printf.sprintf "L%s%s;" (encode_path (pack, name)) (encode_params params)
  | TObjectInner (pack, inners) ->
    Printf.sprintf "L%s.%s;"
      (String.concat "/" pack)
      (String.concat "." (List.map (fun (name, params) ->
        name ^ (encode_params params)
      ) inners))
  | TMethod (args, ret) ->
    let ret =
      match ret with
      | None   -> "V"
      | Some v -> encode_ty v
    in
    Printf.sprintf "(%s)%s" (String.concat "" (List.map (encode_ty) args)) ret
  | TTypeParameter s -> Printf.sprintf "T%s;" s

let rec const ctx c =
  begin try
    PMap.find c ctx.consts
  with
  | Not_found ->
    let n = write_const ctx c in

    let ret = ctx.ccount in
    ctx.ccount <- ret + n;
    ctx.consts <- PMap.add c ret ctx.consts;
    ret
  end
and write_const ctx = function
      (** references a class or an interface - jpath must be encoded as StringUtf8 *)
      | ConstClass path -> (* tag = 7 *)
          let path = const ctx (ConstUtf8 (encode_path path)) in
          write_byte ctx.cpool 7;
          write_ui16 ctx.cpool path;
          1
      (** field reference *)
      | ConstField (jpath, string, jty) (* tag = 9 *) ->
          let jpath = const ctx (ConstClass jpath) in
          let name = const ctx (ConstNameAndType (string, jty)) in
          write_byte ctx.cpool 9;
          write_ui16 ctx.cpool jpath;
          write_ui16 ctx.cpool name;
          1
      (** method reference; string can be special "<init>" and "<clinit>" values *)
      | ConstMethod (jpath, string, jmty) (* tag = 10 *) ->
          let jpath = const ctx (ConstClass jpath) in
          let name = const ctx (ConstNameAndType (string, TMethod jmty)) in
          write_byte ctx.cpool 10;
          write_ui16 ctx.cpool jpath;
          write_ui16 ctx.cpool name;
          1
      
      (** interface method reference *)
      | ConstInterfaceMethod (jpath, string, jmty) (* tag = 11 *) ->
          let jpath = const ctx (ConstClass jpath) in
          let name = const ctx (ConstNameAndType (string, TMethod jmty)) in
          write_byte ctx.cpool 11;
          write_ui16 ctx.cpool jpath;
          write_ui16 ctx.cpool name;
          1

      (** constant values *)
      | ConstString s  (* tag = 8 *) ->
          let s = const ctx (ConstUtf8 s) in
          write_byte ctx.cpool 8;
          write_ui16 ctx.cpool s;
          1
      | ConstInt i (* tag = 3 *) ->
          write_byte ctx.cpool 3;
          write_real_i32 ctx.cpool i;
          1
      | ConstFloat f (* tag = 4 *) ->
          write_byte ctx.cpool 4;
      (*    (match classify_float f with
          | FP_normal | FP_subnormal | FP_zero ->*)
              write_real_i32 ctx.cpool (Int32.bits_of_float f);
      (*    | FP_infinity when f > 0 ->
              write_real_i32 ctx.cpool 0x7f800000l
          | FP_infinity when f < 0 ->
              write_real_i32 ctx.cpool 0xff800000l
          | FP_nan ->
              write_real_i32 ctx.cpool 0x7f800001l)
      *)
          1
      | ConstLong i (* tag = 5 *) ->
          write_byte ctx.cpool 5;
          write_i64 ctx.cpool i;
          2
      | ConstDouble d (* tag = 6 *) ->
          write_byte ctx.cpool 6;
          write_double ctx.cpool d;
          2
      (** name and type: used to represent a field or method, without indicating which class it belongs to *)
      | ConstNameAndType (string, jty) ->
          let name = (const ctx (ConstUtf8 (string))) in
          let jty = (const ctx (ConstUtf8 (encode_ty jty))) in
          write_byte ctx.cpool 12;
          write_ui16 ctx.cpool name;
          write_ui16 ctx.cpool jty;
          1
      (** UTF8 encoded strings. Note that when reading/writing, take into account Utf8 modifications of java *)
      (* (http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.4.7) *)
      | ConstUtf8 s ->
          write_byte ctx.cpool 1;
          write_ui16 ctx.cpool (String.length s);
          nwrite ctx.cpool (encode_utf8 s);
          1
      (** invokeDynamic-specific *)
      | ConstMethodHandle (ref_type, jconst) (* tag = 15 *) ->
          write_byte ctx.cpool 15;
          write_byte ctx.cpool (get_ref_type ref_type);
          write_ui16 ctx.cpool (const ctx jconst);
          1
      | ConstMethodType jmty (* tag = 16 *) ->
          let jty = (const ctx (ConstUtf8 (encode_ty (TMethod jmty)))) in
          write_byte ctx.cpool 16;
          write_ui16 ctx.cpool jty;
          1
      | ConstInvokeDynamic (bootstrap_method, string, jty) (* tag = 18 *) ->
          let name_and_type = const ctx (ConstNameAndType(string, jty)) in
          write_byte ctx.cpool 18;
          write_ui16 ctx.cpool bootstrap_method;
          write_ui16 ctx.cpool name_and_type;
          1
      | ConstUnusable -> 1

let ctx_to_consts ctx =
  let arr = Array.create ctx.ccount ConstUnusable in
  PMap.iter (fun c i ->
    arr.(i) <- c
  ) ctx.consts;
  arr

let new_ctx consts =
  let map = ref PMap.empty in
  let consts = if Array.length consts = 0 then [|ConstUnusable|] else consts in
  Array.iteri (fun i c ->
    map := PMap.add c i !map;
  ) consts;
  let ctx = {
    cpool = output_string ();
    ccount = Array.length consts;
    ch = output_string ();
    consts = !map;
  } in
  Array.iter (fun c ->
    ignore(write_const ctx c)
  ) consts;
  ctx.ccount = Array.length consts;
  ctx
let open_ctx ctx f =
  let back_ch = ctx.ch in
  ctx.ch <- output_string ();
  let r = f ctx in
  ctx.ch <- back_ch;
  r

(* Acess (and other) flags unparsing *)
(*************************************)

let encode_access accs =
  List.fold_left begin fun acc -> function
    | JPublic       -> acc lor 0x0001
    | JPrivate      -> acc lor 0x0002
    | JProtected    -> acc lor 0x0004
    | JStatic       -> acc lor 0x0008
    | JFinal        -> acc lor 0x0010
    | JSynchronized -> acc lor 0x0020
    | JVolatile     -> acc lor 0x0040
    | JTransient    -> acc lor 0x0080
    (* added if created by the compiler *)
    | JSynthetic    -> acc lor 0x1000
    | JEnum         -> acc lor 0x4000
    | JUnusable     -> acc (* should not be present *)
    (* class flags *)
    | JSuper        -> acc lor 0x0020
    | JInterface    -> acc lor 0x0200
    | JAbstract     -> acc lor 0x0400
    | JAnnotation   -> acc lor 0x2000
    (* method flags *)
    | JBridge       -> acc lor 0x0040
    | JVarArgs      -> acc lor 0x0080
    | JNative       -> acc lor 0x0100
    | JStrict       -> acc lor 0x0800
  end 0 accs

(* Attributes unparsing *)
(************************)

(*
let verification_type ctx vtype =
  match vtype with
    | VTop  -> write_byte ctx.ch 0
    | VInteger  -> write_byte ctx.ch 1
    | VFloat -> write_byte ctx.ch 2
    | VDouble -> write_byte ctx.ch 3
    | VLong -> write_byte ctx.ch 4
    | VNull -> write_byte ctx.ch 5
    | VUninitializedThis -> write_byte ctx.ch 6
    | VObject o ->
    write_byte ctx.ch 7 ; write_object_type ctx.ctx o
    | VUninitialized pc -> write_byte ctx.ch 8 ; write_ui16 ctx.ch pc

let verification_type_list ctx =
  write_with_size write_ui16 ch (verification_type ctx)

let stackmap_attr consts stackmap =
  let ch = output_string ()
  in
    write_with_size write_ui16 ch
      (function pc, lt, st ->
     write_ui16 ch pc;
     verification_type_list ctx lt;
     verification_type_list ctx st)
      stackmap;
    ("StackMap", close_out ch)

let stackmap_table_attr consts stackmap_attr =
  let ch = output_string ()
  in
    write_with_size write_ui16 ch
      (function stackmap_frame ->
     match stackmap_frame with
       | SameFrame k ->
           write_byte ch k
       | SameLocals (k, vtype) ->
           write_byte ch k;
           verification_type ctx vtype
       | SameLocalsExtended (k, offset_delta, vtype) ->
           write_byte ch k;
           write_ui16 ch offset_delta;
           verification_type ctx vtype
       | ChopFrame (k, offset_delta) ->
           write_byte ch k;
           write_ui16 ch offset_delta
       | SameFrameExtended (k, offset_delta) ->
           write_byte ch k;
           write_ui16 ch offset_delta
       | AppendFrame (k, offset_delta, vtype_list) ->
           (* The vtype list is not dumped with verification_type_list
          because its length doesn't need to be dumped. Indeed it is
          deduced from k (it is k-251). *)
           write_byte ch k;
           write_ui16 ch offset_delta;
           List.iter (verification_type ctx) vtype_list
       | FullFrame (k, offset_delta, lvtypes, svtypes) ->
           write_byte ch k;
           write_ui16 ch offset_delta;
           verification_type_list ctx lvtypes;
           verification_type_list ctx svtypes
      ) stackmap_attr;
    ("StackMapTable", close_out ch)
*)

let write_utf8 ctx str = 
  write_ui16 ctx.ch (const ctx (ConstUtf8 str))

let rec encode_val ctx v =
  begin match v with
    | ValConst(c, cst) ->
        write_byte ctx.ch c;
        write_ui16 ctx.ch (const ctx cst)
    | ValEnum (cn, constructor) ->
        write_byte ctx.ch (Char.code 'e');
        write_utf8 ctx (encode_ty cn);
        write_utf8 ctx constructor
    | ValClass sign ->
        write_byte ctx.ch (Char.code 'c');
        write_utf8 ctx (encode_ty sign)
    | ValAnnotation annot ->
        write_byte ctx.ch (Char.code '@');
        encode_annot ctx annot
    | ValArray elements ->
        write_byte ctx.ch (Char.code '[');
        write_ui16 ctx.ch (List.length elements);
        List.iter (encode_val ctx) elements
  end

and encode_annot ctx annot =
    write_utf8 ctx (encode_ty annot.ann_type);
    write_ui16 ctx.ch (List.length annot.ann_elems);
    List.iter begin fun (name, value) ->
      write_utf8 ctx name;
      encode_val ctx value
    end annot.ann_elems

let encode_annots ctx annots =
  write_ui16 ctx.ch (List.length annots);
  List.iter (encode_annot ctx) annots

let encode_parameter_annots ctx param_annots =
  write_byte ctx.ch (List.length param_annots);
  List.iter (encode_annots ctx) param_annots

let rec encode_attr_to_strings ctx = function
      | AttrDeprecated -> ("Deprecated", "")
      | AttrUnknown (name, contents) -> (name, contents)
      | AttrVisibleAnnotations annots ->
          open_ctx ctx (fun ctx ->
            encode_annots ctx annots;
            ("RuntimeVisibleAnnotations", close_out ctx.ch)
          )
      | AttrInvisibleAnnotations annots ->
          open_ctx ctx (fun ctx ->
            encode_annots ctx annots;
            ("RuntimeInvisibleAnnotations", close_out ctx.ch)
          )
      | AttrLineNumberTable l ->
          open_ctx ctx (fun ctx ->
            write_ui16 ctx.ch (List.length l);
            List.iter(fun (pc, line) ->
               write_ui16 ctx.ch pc;
               write_ui16 ctx.ch line
            ) l;
            ("LineNumberTable", close_out ctx.ch)
          )
      | AttrSourceFile s ->
          open_ctx ctx (fun ctx ->
            write_utf8 ctx s;
            ("SourceFile", close_out ctx.ch)
          )
      (*
      | AttrSynthetic ->
      ("Synthetic", close_out ch)
      | AttrVisibleParameterAnnotations param_annots ->
          encode_parameter_annots ctx param_annots;
          ("RuntimeVisibleParameterAnnotations", close_out ch)
      | AttrInvisibleParameterAnnotations param_annots ->
          encode_parameter_annots ctx param_annots;
          ("RuntimeInvisibleParameterAnnotations", close_out ch)
      *)
      (*
      | AttrEnclosingMethod (cn, mso) ->
      write_class ctx cn;
      (match mso with
         | Some (n, t) ->
                 write_name_and_type ctx (n, t)
         | None ->
         write_ui16 ch 0);
      ("EnclosingMethod", close_out ch)
      | AttrSourceDebugExtension s ->
      ("SourceDebugExtension", s)
      | AttrConstant c ->
      write_value ctx c;
      ("ConstantValue", close_out ch)
      | AttrLocalVariableTable l ->
      write_with_size write_ui16 ch
        (function start_pc, length, name, signature, index ->
           write_ui16 ch start_pc;
           write_ui16 ch length;
           write_string ctx name;
           write_string ctx (encode_ty signature);
           write_ui16 ch index)
        l;
      ("LocalVariableTable", close_out ch)
      | AttrLocalVariableTypeTable l ->
          write_with_size write_ui16 ch
            (function start_pc, length, name, signature, index ->
               write_ui16 ch start_pc;
               write_ui16 ch length;
               write_string ctx name;
               write_string ctx (encode_FieldTypeSignature signature);
               write_ui16 ch index)
            l;
          ("LocalVariableTypeTable", close_out ch)
      *)

      (*
      | AttrStackMap s ->
      ignore (close_out ch);
      stackmap_attr consts s
      | AttrStackMapTable s ->
      ignore (close_out ch);
      stackmap_table_attr consts s
      | AttrAnnotationDefault ev ->
          encode_val ctx ev;
          ("AnnotationDefault", close_out ch)
      *)

and encode_attr ctx attr =
  let (name, content) = encode_attr_to_strings ctx attr in
  debug "encode_attr (%S, %S)@." name content;
  write_utf8 ctx name;
  write_i32 ctx.ch (String.length content);
  nwrite ctx.ch content

let encode_field ctx field =
  write_ui16 ctx.ch (encode_access field.jf_accs);
  write_utf8 ctx field.jf_name;
  write_utf8 ctx (encode_ty field.jf_vmty);
  let attrs = field.jf_attrs in
  let attrs =
    begin match field.jf_const with
    | None -> attrs
    | Some(c) ->
      open_ctx ctx (fun ctx ->
        write_ui16 ctx.ch (const ctx c);
        AttrUnknown("ConstantValue", close_out ctx.ch) :: attrs
      )
    end
  in
  let attrs =
    if field.jf_ty = field.jf_vmty then attrs else
    open_ctx ctx (fun ctx ->
      write_ui16 ctx.ch (const ctx (ConstUtf8 (encode_ty field.jf_ty)));
      AttrUnknown("Signature", close_out ctx.ch) :: attrs
    )
  in
  write_ui16 ctx.ch (List.length attrs);
  List.iter (encode_attr ctx) attrs

let encode_method ctx field =
  write_ui16 ctx.ch (encode_access field.jf_accs);
  write_utf8 ctx field.jf_name;
  write_utf8 ctx (encode_ty field.jf_ty);
  let attrs = field.jf_attrs in
  let attrs =
    let throws = field.jf_throws in
    let len = List.length throws in
    if len = 0 then attrs else
    open_ctx ctx (fun ctx ->
      write_ui16 ctx.ch len;
      List.iter (function
        | TObject(e, _) ->
          let n = const ctx (ConstClass e) in
          write_ui16 ctx.ch n
        | _ -> assert false   
      ) throws;
      AttrUnknown("Exceptions", close_out ctx.ch) :: attrs
    )
  in
  let attrs =
    if field.jf_ty = field.jf_vmty then attrs else
    open_ctx ctx (fun ctx ->
      write_ui16 ctx.ch (const ctx (ConstUtf8 (encode_ty field.jf_ty)));
      AttrUnknown("Signature", close_out ctx.ch) :: attrs
    )
  in
  write_ui16 ctx.ch (List.length attrs);
  List.iter (encode_attr ctx) attrs

let encode_class ch c =
  write_real_i32 ch 0xCAFEBABEl;
  write_ui16 ch (snd c.cversion);
  write_ui16 ch (fst c.cversion);

  let ctx = new_ctx c.consts in

  write_ui16 ctx.ch (encode_access c.caccs);
  write_ui16 ctx.ch (const ctx (ConstClass c.cpath));
  begin match c.csuper with
    | TObject(path, []) -> write_ui16 ctx.ch (const ctx (ConstClass path))
    | _ -> assert false
  end;
  write_ui16 ctx.ch (List.length c.cinterfaces);
  List.iter (function
    | TObject(path, []) -> write_ui16 ctx.ch (const ctx (ConstClass path))
    | v -> Format.printf "error interface type %a@." JDataPP.pp_jty v; assert false
  ) c.cinterfaces;

  write_ui16 ctx.ch (List.length c.cfields);
  List.iter (fun field -> encode_field ctx field) c.cfields;

  write_ui16 ctx.ch (List.length c.cmethods);
  List.iter (fun field -> encode_method ctx field) c.cmethods;
  
  let inner = open_ctx ctx (fun ctx ->
    write_ui16 ctx.ch (List.length c.cinner_types);
    List.iter (fun (inner, outer, inner_name, accs) ->
      let n = const ctx (ConstClass inner) in
      write_ui16 ctx.ch n;

      begin match outer with
      | None -> write_ui16 ctx.ch 0
      | Some path -> let n = const ctx (ConstClass path) in write_ui16 ctx.ch n
      end;
      begin match inner_name with
      | None -> write_ui16 ctx.ch 0
      | Some name -> write_ui16 ctx.ch (const ctx (ConstUtf8 name))
      end;
      write_ui16 ctx.ch (encode_access accs);
    ) c.cinner_types;
    AttrUnknown("InnerClasses", close_out ctx.ch)
  ) in

  let attrs = c.cattrs @ [inner] in
  write_ui16 ctx.ch (List.length attrs);
  List.iter (fun attr -> encode_attr ctx attr) attrs;

  write_ui16 ch ctx.ccount;
  nwrite ch (close_out ctx.cpool);
  nwrite ch (close_out ctx.ch)
