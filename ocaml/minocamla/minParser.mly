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

let reloc_exp x = { x with pexp_loc = symbol_rloc () };;

let mkoperator name pos =
  let loc = rhs_loc pos in
  Exp.mk ~loc (Pexp_ident(mkloc (Lident name) loc))

let mkinfix arg1 name arg2 =
  mkexp(Pexp_apply(mkoperator name 2, ["", arg1; "", arg2]))

let mkstrexp e attrs =
  {
    pstr_desc = Pstr_eval (e, attrs);
    pstr_loc = e.pexp_loc
  }


%}

/* Tokens */
%token EOF
%token <int> INT
%token <string> LIDENT
%token LPAREN
%token MINUS
%token OPEN
%token PLUS
%token RPAREN
%token SEMISEMI
%token STAR
%token <string * string option> STRING
%token <string> UIDENT
%token UNDERSCORE
%token <string * Location.t> COMMENT

%token EOL

%left     PLUS MINUS                    /* expr (e OP e OP e) */
%left     STAR                          /* expr (e OP e OP e) */
%nonassoc prec_unary_minus              /* unary - */

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
  | expr                                  { [mkstrexp $1 []] }
  | top_structure_tail                    { $1 }
top_structure_tail:
  |                                       { [] }
  | structure_item top_structure_tail     { $1 :: $2 }
use_file:
  | use_file_tail                         { $1 }
use_file_tail:
  | EOF                                   { [] }
  | structure_item use_file_tail          { Ptop_def[$1] :: $2 }
parse_core_type:
  |                                       { mktyp(Ptyp_any) }
parse_expression:
  | expr EOF                              { $1 }
parse_pattern:
  | EOF                                   { mkpat(Ppat_any) }

structure:
  | expr structure_tail           { mkstrexp $1 [] :: $2 }
  | structure_tail                { $1 }
structure_tail:
  |                               { [] }
  | SEMISEMI structure            { $2 }
  | structure_item structure_tail { $1 :: $2 }
structure_item:
  | OPEN UIDENT { mkstr(Pstr_open(Opn.mk (mkrhs (Lident $2) 3) ~loc:(symbol_rloc()))) }

/* Core expressions */

expr:
  | simple_expr         { $1 }
  | simple_expr simple_labeled_expr_list { mkexp(Pexp_apply($1, List.rev $2)) }
  | expr PLUS       expr { mkinfix $1 "+" $3 }
  | expr MINUS      expr { mkinfix $1 "-" $3 }
  | expr STAR       expr { mkinfix $1 "*" $3 }
simple_expr:
  | LIDENT                                   { mkexp(Pexp_ident (mkrhs (Lident $1) 1)) }
  | INT                                      { mkexp(Pexp_constant(Const_int $1)) }
  | STRING                                   { mkexp(Pexp_constant(Const_string (fst $1, snd $1))) }
  | UIDENT                                   { mkexp(Pexp_construct(mkrhs (Lident $1) 1, None)) }
  | LPAREN RPAREN                            { mkexp(Pexp_construct(mkrhs (Lident "()") 1, None)) }

  | LPAREN expr RPAREN                       { reloc_exp $2 }
simple_labeled_expr_list:
  | simple_expr                              { [("",$1)] }
  | simple_labeled_expr_list simple_expr     { ("", $2) :: $1 }

%%
