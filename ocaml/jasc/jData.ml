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

type jpath = (string list) * string

type jty_arg =
  | TExtends of jty (* + *)
  | TSuper of jty (* - *)
  | TType of jty
  | TAny (* * *)

and jty =
  | TByte (* B *)
  | TChar (* C *)
  | TDouble (* D *)
  | TFloat (* F *)
  | TInt (* I *)
  | TLong (* J *)
  | TShort (* S *)
  | TBool (* Z *)
  | TObject of jpath * jty_arg list (* L Classname *)
  | TObjectInner of (string list) * (string * jty_arg list) list (* L Classname ClassTypeSignatureSuffix *)
  | TArray of jty * int option (* [ *)
  | TMethod of jmty (* ( *)
  | TTypeParameter of string (* T *)

(* ( jty list ) ReturnDescriptor (| V | jty) *)
and jmty = jty list * jty option

(* InvokeDynamic-specific: Method handle *)
type ref_type =
  | RGetField (* constant must be ConstField *)
  | RGetStatic (* constant must be ConstField *)
  | RPutField (* constant must be ConstField *)
  | RPutStatic (* constant must be ConstField *)
  | RInvokeVirtual (* constant must be Method *)
  | RInvokeStatic (* constant must be Method *)
  | RInvokeSpecial (* constant must be Method *)
  | RNewInvokeSpecial (* constant must be Method with name <init> *)
  | RInvokeInterface (* constant must be InterfaceMethod *)

(* TODO *)
type bootstrap_method = int

type jconst =
  (** references a class or an interface - jpath must be encoded as StringUtf8 *)
  | ConstClass of jpath (* tag = 7 *)
  (** field reference *)
  | ConstField of (jpath * string * jty) (* tag = 9 *)
  (** method reference; string can be special "<init>" and "<clinit>" values *)
  | ConstMethod of (jpath * string * jmty) (* tag = 10 *)
  (** interface method reference *)
  | ConstInterfaceMethod of (jpath * string * jmty) (* tag = 11 *)
  (** constant values *)
  | ConstString of string  (* tag = 8 *)
  | ConstInt of int32 (* tag = 3 *)
  | ConstFloat of float (* tag = 4 *)
  | ConstLong of int64 (* tag = 5 *)
  | ConstDouble of float (* tag = 6 *)
  (** name and type: used to represent a field or method, without indicating which class it belongs to *)
  | ConstNameAndType of string * jty
  (** UTF8 encoded strings. Note that when reading/writing, take into account Utf8 modifications of java *)
  (* (http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.4.7) *)
  | ConstUtf8 of string
  (** invokeDynamic-specific *)
  | ConstMethodHandle of (ref_type * jconst) (* tag = 15 *)
  | ConstMethodType of jmty (* tag = 16 *)
  | ConstInvokeDynamic of (bootstrap_method * string * jty) (* tag = 18 *)
  | ConstUnusable

type jcode = unit (* TODO *)

type jaccess =
  | JPublic (* 0x0001 *)
  | JPrivate (* 0x0002 *) 
  | JProtected (* 0x0004 *)
  | JStatic (* 0x0008 *)
  | JFinal (* 0x0010 *)
  | JSynchronized (* 0x0020 *)
  | JVolatile (* 0x0040 *)
  | JTransient (* 0x0080 *)
  (** added if created by the compiler *)
  | JSynthetic (* 0x1000 *)
  | JEnum (* 0x4000 *)
  | JUnusable (* should not be present *)
  (** class flags *)
  | JSuper (* 0x0020 *)
  | JInterface (* 0x0200 *)
  | JAbstract (* 0x0400 *)
  | JAnnotation (* 0x2000 *)
  (** method flags *)
  | JBridge (* 0x0040 *)
  | JVarArgs (* 0x0080 *)
  | JNative (* 0x0100 *)
  | JStrict (* 0x0800 *)

(* type parameter name, extends signature, implements signatures *)
type jtypes = (string * jty option * jty list) list

type jannotation = {
  ann_type : jty;
  ann_elements : (string * jannotation_value) list;
}

and jannotation_value =
  | ValConst of int * jconst (* B, C, D, E, F, I, J, S, Z, s *)
  | ValEnum of jty * string (* e *)
  | ValClass of jty (* c *) (* V -> Void *)
  | ValAnnotation of jannotation (* @ *)
  | ValArray of jannotation_value list (* [ *)

type jattr =
  | AttrDeprecated
  | AttrVisibleAnnotations of jannotation list
  | AttrInvisibleAnnotations of jannotation list
  | AttrUnknown of string * string

type jfield_kind =
  | JKField
  | JKMethod

type jfield = {
  jf_name : string;
  jf_kind : jfield_kind;
  (* signature, as used by the vm *)
  jf_vmty : jty;
  (* actual signature, as used in java code *)
  jf_ty : jty;
  jf_throws : jty list;
  jf_types : jtypes;
  jf_accs : jaccess list;
  jf_attrs : jattr list;
  jf_const : jconst option;
  jf_code : jcode option;
}

type jclass = {
  cversion : int * int;
  consts : jconst array;
  cpath : jpath;
  csuper : jty;
  caccs : jaccess list;
  cinterfaces : jty list;
  cfields : jfield list;
  cmethods : jfield list;
  cattrs : jattr list;
  cinner_types : (jpath * jpath option * string option * jaccess list) list;
  ctypes : jtypes;
}

let debug fmt = Format.printf fmt
let debug0 fmt =
    Format.kfprintf
      (fun ppf -> ())
      (Format.formatter_of_buffer (Buffer.create 16))
      fmt
