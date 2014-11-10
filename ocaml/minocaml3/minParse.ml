(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Entry points in the parser *)

(* Skip tokens to the end of the phrase *)

let rec skip_phrase lexbuf =
  try
    match MinLexer.token lexbuf with
    | MinParser.SEMISEMI
    | MinParser.EOF -> ()
    | _ -> skip_phrase lexbuf
  with
    | MinLexer.Error (MinLexer.Unterminated_comment _, _) -> ()
    | MinLexer.Error (MinLexer.Unterminated_string, _) -> ()
    | MinLexer.Error (MinLexer.Unterminated_string_in_comment _, _) -> ()
    | MinLexer.Error (MinLexer.Illegal_character _, _) -> skip_phrase lexbuf
;;

let maybe_skip_phrase lexbuf =
  if Parsing.is_current_lookahead MinParser.SEMISEMI
  || Parsing.is_current_lookahead MinParser.EOF
  then ()
  else skip_phrase lexbuf

let wrap parsing_fun lexbuf =
  try
    MinLexer.init ();
    let ast = parsing_fun MinLexer.token lexbuf in
    Parsing.clear_parser();
    ast
  with
  | MinLexer.Error(MinLexer.Unterminated_comment _, _) as err -> raise err
  | MinLexer.Error(MinLexer.Unterminated_string, _) as err -> raise err
  | MinLexer.Error(MinLexer.Unterminated_string_in_comment _, _) as err -> raise err
  | MinLexer.Error(MinLexer.Illegal_character _, _) as err ->
      if !Location.input_name = "//toplevel//" then skip_phrase lexbuf;
      raise err
  | Syntaxerr.Error _ as err ->
      if !Location.input_name = "//toplevel//" then maybe_skip_phrase lexbuf;
      raise err
  | Parsing.Parse_error | Syntaxerr.Escape_error ->
      let loc = Location.curr lexbuf in
      if !Location.input_name = "//toplevel//"
      then maybe_skip_phrase lexbuf;
      raise(Syntaxerr.Error(Syntaxerr.Other loc))
;;

let implementation = wrap MinParser.implementation
and interface = wrap MinParser.interface
and toplevel_phrase = wrap MinParser.toplevel_phrase
and use_file = wrap MinParser.use_file
