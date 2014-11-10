/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

/* The parser definition */

%{
open Location
open Asttypes
open Longident
open Parsetree
open Ast_helper

let mktyp d = Typ.mk ~loc:(symbol_rloc()) d
let mkpat d = Pat.mk ~loc:(symbol_rloc()) d
let mkexp d = Exp.mk ~loc:(symbol_rloc()) d
let mkstr d = Str.mk ~loc:(symbol_rloc()) d

let mkrhs rhs pos = mkloc rhs (rhs_loc pos)

let reloc_pat x = { x with ppat_loc = symbol_rloc () };;
let reloc_exp x = { x with pexp_loc = symbol_rloc () };;

let mkoperator name pos =
  let loc = rhs_loc pos in
  Exp.mk ~loc (Pexp_ident(mkloc (Lident name) loc))

let ghexp d = Exp.mk ~loc:(symbol_gloc ()) d

let mkinfix arg1 name arg2 =
  mkexp(Pexp_apply(mkoperator name 2, ["", arg1; "", arg2]))

let neg_float_string f =
  if String.length f > 0 && f.[0] = '-'
  then String.sub f 1 (String.length f - 1)
  else "-" ^ f

let mkuminus name arg =
  match name, arg.pexp_desc with
  | "-", Pexp_constant(Const_int n) ->
      mkexp(Pexp_constant(Const_int(-n)))
  | "-", Pexp_constant(Const_int32 n) ->
      mkexp(Pexp_constant(Const_int32(Int32.neg n)))
  | "-", Pexp_constant(Const_int64 n) ->
      mkexp(Pexp_constant(Const_int64(Int64.neg n)))
  | "-", Pexp_constant(Const_nativeint n) ->
      mkexp(Pexp_constant(Const_nativeint(Nativeint.neg n)))
  | ("-" | "-."), Pexp_constant(Const_float f) ->
      mkexp(Pexp_constant(Const_float(neg_float_string f)))
  | _ ->
      mkexp(Pexp_apply(mkoperator ("~" ^ name) 1, ["", arg]))

let mkstrexp e attrs =
  {
    pstr_desc = Pstr_eval (e, attrs);
    pstr_loc = e.pexp_loc
  }

let wrap_exp_attrs body (ext, attrs) =
  (* todo: keep exact location for the entire attribute *)
  let body = {body with pexp_attributes = attrs @ body.pexp_attributes} in
  match ext with
  | None -> body
  | Some id -> ghexp(Pexp_extension (id, PStr [mkstrexp body []]))

let mkexp_attrs d attrs =
  wrap_exp_attrs (mkexp d) attrs

%}

/* Tokens */

%token AMPERAMPER
%token AMPERSAND
%token AND
%token AS
%token ASSERT
%token BACKQUOTE
%token BANG
%token BAR
%token BARBAR
%token BARRBRACKET
%token BEGIN
%token <char> CHAR
%token CLASS
%token COLON
%token COLONCOLON
%token COLONEQUAL
%token COLONGREATER
%token COMMA
%token CONSTRAINT
%token DO
%token DONE
%token DOT
%token DOTDOT
%token DOWNTO
%token ELSE
%token END
%token EOF
%token EQUAL
%token EXCEPTION
%token EXTERNAL
%token FALSE
%token <string> FLOAT
%token FOR
%token FUN
%token FUNCTION
%token FUNCTOR
%token GREATER
%token GREATERRBRACE
%token GREATERRBRACKET
%token IF
%token IN
%token INCLUDE
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token INHERIT
%token INITIALIZER
%token <int> INT
%token <int32> INT32
%token <int64> INT64
%token <string> LABEL
%token LAZY
%token LBRACE
%token LBRACELESS
%token LBRACKET
%token LBRACKETBAR
%token LBRACKETLESS
%token LBRACKETGREATER
%token LBRACKETPERCENT
%token LBRACKETPERCENTPERCENT
%token LESS
%token LESSMINUS
%token LET
%token <string> LIDENT
%token LPAREN
%token LBRACKETAT
%token LBRACKETATAT
%token LBRACKETATATAT
%token MATCH
%token METHOD
%token MINUS
%token MINUSDOT
%token MINUSGREATER
%token MODULE
%token MUTABLE
%token <nativeint> NATIVEINT
%token NEW
%token OBJECT
%token OF
%token OPEN
%token <string> OPTLABEL
%token OR
/* %token PARSER */
%token PERCENT
%token PLUS
%token PLUSDOT
%token PLUSEQ
%token <string> PREFIXOP
%token PRIVATE
%token QUESTION
%token QUOTE
%token RBRACE
%token RBRACKET
%token REC
%token RPAREN
%token SEMI
%token SEMISEMI
%token SHARP
%token SIG
%token STAR
%token <string * string option> STRING
%token STRUCT
%token THEN
%token TILDE
%token TO
%token TRUE
%token TRY
%token TYPE
%token <string> UIDENT
%token UNDERSCORE
%token VAL
%token VIRTUAL
%token WHEN
%token WHILE
%token WITH
%token <string * Location.t> COMMENT

%token EOL

%nonassoc below_SEMI
%nonassoc SEMI                          /* below EQUAL ({lbl=...; lbl=...}) */
%left     EQUAL                         /* expr (e OP e OP e) */
%left     PLUS MINUS                    /* expr (e OP e OP e) */
%left     STAR                          /* expr (e OP e OP e) */
%nonassoc prec_unary_minus              /* unary - */
/* Finally, the first tokens of simple_expr are above everything else. */

/* Entry points */

%start implementation                   /* for implementation files */
%type <Parsetree.structure> implementation
%start interface                        /* for interface files */
%type <Parsetree.signature> interface
%start toplevel_phrase                  /* for interactive use */
%type <Parsetree.toplevel_phrase> toplevel_phrase
%start use_file                         /* for the #use directive */
%type <Parsetree.toplevel_phrase list> use_file
%start parse_core_type
%type <Parsetree.core_type> parse_core_type
%start parse_expression
%type <Parsetree.expression> parse_expression
%start parse_pattern
%type <Parsetree.pattern> parse_pattern
%%

/* Entry points */

implementation:
  | structure EOF                         { $1 }
interface:
  | EOF                                   { [] }
toplevel_phrase:
  | top_structure SEMISEMI                { Ptop_def $1 }
  | EOF                                   { raise End_of_file }
top_structure:
  | seq_expr                              { [mkstrexp $1 []] }
  | top_structure_tail                    { $1 }
top_structure_tail:
  |                                       { [] }
  | structure_item top_structure_tail     { $1 :: $2 }
use_file:
  | use_file_tail                         { $1 }
  | seq_expr use_file_tail
                                          { Ptop_def[mkstrexp $1 []] :: $2 }
use_file_tail:
  | EOF                                   { [] }
  | structure_item use_file_tail          { Ptop_def[$1] :: $2 }
parse_core_type:
  |                                       { mktyp(Ptyp_any) }
parse_expression:
  | seq_expr EOF                          { $1 }
parse_pattern:
  | pattern EOF                           { $1 }


structure:
  | seq_expr structure_tail       { mkstrexp $1 [] :: $2 }
  | structure_tail                { $1 }
structure_tail:
  |                               { [] }
  | structure_item structure_tail { $1 :: $2 }
structure_item:
  | LET let_bindings { mkstr(Pstr_value(Nonrecursive, List.rev $2)) }
  | OPEN UIDENT { mkstr(Pstr_open(Opn.mk (mkrhs (Lident $2) 3) ~loc:(symbol_rloc()))) }

/* Core expressions */

seq_expr:
  | expr        %prec below_SEMI  { $1 }
  | expr SEMI seq_expr            { mkexp(Pexp_sequence($1, $3)) }
expr:
  | simple_expr         { $1 }
  | simple_expr simple_labeled_expr_list { mkexp(Pexp_apply($1, List.rev $2)) }
  | LET let_bindings IN seq_expr
      { mkexp_attrs (Pexp_let(Nonrecursive, List.rev $2, $4)) (None, []) }
  | expr PLUS       expr { mkinfix $1 "+" $3 }
  | expr MINUS      expr { mkinfix $1 "-" $3 }
  | expr STAR       expr { mkinfix $1 "*" $3 }
  | expr EQUAL      expr { mkinfix $1 "=" $3 }
  | MINUS    expr %prec prec_unary_minus { mkuminus "-" $2 }
simple_expr:
  | LIDENT                                           { mkexp(Pexp_ident (mkrhs (Lident $1) 1)) }
  | INT                                              { mkexp(Pexp_constant(Const_int $1)) }
  | STRING                                           { mkexp(Pexp_constant(Const_string (fst $1, snd $1))) }
  | constr_longident  { mkexp(Pexp_construct(mkrhs $1 1, None)) }
  | LPAREN seq_expr RPAREN                           { reloc_exp $2 }
simple_labeled_expr_list:
  | simple_expr               { [("",$1)] }
  | simple_labeled_expr_list simple_expr     { ("", $2) :: $1 }
let_bindings:
  | pattern EQUAL seq_expr        { [Vb.mk ~loc:(symbol_rloc()) $1 $3] }

/* Patterns */

pattern:
  | LIDENT                                      { mkpat(Ppat_var (mkrhs $1 1)) }
  | UNDERSCORE                                  { mkpat(Ppat_any) }
  | constr_longident                            { mkpat(Ppat_construct(mkrhs $1 1, None)) }
  | LPAREN pattern RPAREN                       { reloc_pat $2 }

/* Identifiers and long identifiers */

constr_longident:
  | UIDENT                                      { Lident $1 }
  | LPAREN RPAREN                               { Lident "()" }

%%
