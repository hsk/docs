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

let mkpatvar name pos =
  Pat.mk ~loc:(rhs_loc pos) (Ppat_var (mkrhs name pos))

(*
  Ghost expressions and patterns:
  expressions and patterns that do not appear explicitly in the
  source file they have the loc_ghost flag set to true.
  Then the profiler will not try to instrument them and the
  -annot option will not try to display their type.

  Every grammar rule that generates an element with a location must
  make at most one non-ghost element, the topmost one.

  How to tell whether your location must be ghost:
  A location corresponds to a range of characters in the source file.
  If the location contains a piece of code that is syntactically
  valid (according to the documentation), and corresponds to the
  AST node, then the location must be real; in all other cases,
  it must be ghost.
*)
let ghexp d = Exp.mk ~loc:(symbol_gloc ()) d
let ghloc d = { txt = d; loc = symbol_gloc () }

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

let mkuplus name arg =
  let desc = arg.pexp_desc in
  match name, desc with
  | "+", Pexp_constant(Const_int _)
  | "+", Pexp_constant(Const_int32 _)
  | "+", Pexp_constant(Const_int64 _)
  | "+", Pexp_constant(Const_nativeint _)
  | ("+" | "+."), Pexp_constant(Const_float _) -> mkexp desc
  | _ ->
      mkexp(Pexp_apply(mkoperator ("~" ^ name) 1, ["", arg]))

let mkstrexp e attrs =
  {
    pstr_desc = Pstr_eval (e, attrs);
    pstr_loc = e.pexp_loc
  }

let array_function str name =

  ghloc (Ldot(Lident str, (if !Clflags.fast then "unsafe_" ^ name else name)))

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

/* Precedences and associativities.

Tokens and rules have precedences.  A reduce/reduce conflict is resolved
in favor of the first rule (in source file order).  A shift/reduce conflict
is resolved by comparing the precedence and associativity of the token to
be shifted with those of the rule to be reduced.

By default, a rule has the precedence of its rightmost terminal (if any).

When there is a shift/reduce conflict between a rule and a token that
have the same precedence, it is resolved using the associativity:
if the token is left-associative, the parser will reduce; if
right-associative, the parser will shift; if non-associative,
the parser will declare a syntax error.

We will only use associativities with operators of the kind  x * x -> x
for example, in the rules of the form    expr: expr BINOP expr
in all other cases, we define two precedences if needed to resolve
conflicts.

The precedences must be listed from low to high.
*/

%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI                          /* below EQUAL ({lbl=...; lbl=...}) */
%nonassoc LET                           /* above SEMI ( ...; let ... in ...) */
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 /* below BAR  (match ... with ...) */
%nonassoc AND             /* above WITH (module rec A: SIG with ... and ...) */
%nonassoc THEN                          /* below ELSE (if ... then ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
%nonassoc LESSMINUS                     /* below COLONEQUAL (lbl <- x := e) */
%right    COLONEQUAL                    /* expr (e := e := e) */
%nonassoc AS
%left     BAR                           /* pattern (p|p|p) */
%nonassoc below_COMMA
%left     COMMA                         /* expr/expr_comma_list (e,e,e) */
%right    MINUSGREATER                  /* core_type2 (t -> t -> t) */
%right    OR BARBAR                     /* expr (e || e || e) */
%right    AMPERSAND AMPERAMPER          /* expr (e && e && e) */
%nonassoc below_EQUAL
%left     INFIXOP0 EQUAL LESS GREATER   /* expr (e OP e OP e) */
%right    INFIXOP1                      /* expr (e OP e OP e) */
%nonassoc below_LBRACKETAT
%nonassoc LBRACKETAT
%nonassoc LBRACKETATAT
%right    COLONCOLON                    /* expr (e :: e :: e) */
%left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT PLUSEQ /* expr (e OP e OP e) */
%left     PERCENT INFIXOP3 STAR                 /* expr (e OP e OP e) */
%right    INFIXOP4                      /* expr (e OP e OP e) */
%nonassoc prec_unary_minus prec_unary_plus /* unary - */
%nonassoc prec_constant_constructor     /* cf. simple_expr (C versus C x) */
%nonassoc prec_constr_appl              /* above AS BAR COLONCOLON COMMA */
%nonassoc below_SHARP
%nonassoc SHARP                         /* simple_expr/toplevel_directive */
%nonassoc below_DOT
%nonassoc DOT
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc BACKQUOTE BANG BEGIN CHAR FALSE FLOAT INT INT32 INT64
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NEW NATIVEINT PREFIXOP STRING TRUE UIDENT
          LBRACKETPERCENT LBRACKETPERCENTPERCENT


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
  | SEMISEMI EOF                          { [] }
  | SEMISEMI seq_expr use_file_tail
                                          { Ptop_def[mkstrexp $2 []] :: $3 }
  | SEMISEMI structure_item use_file_tail { Ptop_def[$2] :: $3 }
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
  | SEMISEMI structure            { $2 }
  | structure_item structure_tail { $1 :: $2 }
structure_item:
  | LET rec_flag let_bindings
      {
        match $3 with
        | [ {pvb_pat = { ppat_desc = Ppat_any; ppat_loc = _ };
             pvb_expr = exp; pvb_attributes = attrs}] ->
            let exp = wrap_exp_attrs exp (None,[]) in
            mkstr(Pstr_eval (exp, attrs))
        | l ->
            mkstr(Pstr_value($2, List.rev l))
      }
  | OPEN mod_longident
      { mkstr(Pstr_open(Opn.mk (mkrhs $2 3) ~loc:(symbol_rloc()))) }

/* Core expressions */

seq_expr:
  | expr        %prec below_SEMI  { $1 }
  | expr SEMI                     { reloc_exp $1 }
  | expr SEMI seq_expr            { mkexp(Pexp_sequence($1, $3)) }
expr:
  | simple_expr %prec below_SHARP        { $1 }
  | simple_expr simple_labeled_expr_list { mkexp(Pexp_apply($1, List.rev $2)) }
  | expr_comma_list %prec below_COMMA    { mkexp(Pexp_tuple(List.rev $1)) }
  | LET rec_flag let_bindings IN seq_expr
      {
        let l = $3 in
        List.iter
          (fun vb ->
            if vb.pvb_attributes <> [] then
              raise Syntaxerr.(Error(Not_expecting(vb.pvb_loc,"item attribute")))
          )
         l;
        mkexp_attrs (Pexp_let($2, List.rev l, $5)) (None, []) }
  | IF seq_expr THEN expr ELSE expr
      { mkexp_attrs(Pexp_ifthenelse($2, $4, Some $6)) (None, []) }
  | IF seq_expr THEN expr
      { mkexp_attrs (Pexp_ifthenelse($2, $4, None)) (None, []) }
  | expr INFIXOP0   expr { mkinfix $1 $2 $3 }
  | expr INFIXOP1   expr { mkinfix $1 $2 $3 }
  | expr INFIXOP2   expr { mkinfix $1 $2 $3 }
  | expr INFIXOP3   expr { mkinfix $1 $2 $3 }
  | expr INFIXOP4   expr { mkinfix $1 $2 $3 }
  | expr PLUS       expr { mkinfix $1 "+" $3 }
  | expr PLUSDOT    expr { mkinfix $1 "+." $3 }
  | expr PLUSEQ     expr { mkinfix $1 "+=" $3 }
  | expr MINUS      expr { mkinfix $1 "-" $3 }
  | expr MINUSDOT   expr { mkinfix $1 "-." $3 }
  | expr STAR       expr { mkinfix $1 "*" $3 }
  | expr PERCENT    expr { mkinfix $1 "%" $3 }
  | expr EQUAL      expr { mkinfix $1 "=" $3 }
  | expr LESS       expr { mkinfix $1 "<" $3 }
  | expr GREATER    expr { mkinfix $1 ">" $3 }
  | expr OR         expr { mkinfix $1 "or" $3 }
  | expr BARBAR     expr { mkinfix $1 "||" $3 }
  | expr AMPERSAND  expr { mkinfix $1 "&" $3 }
  | expr AMPERAMPER expr { mkinfix $1 "&&" $3 }
  | expr COLONEQUAL expr { mkinfix $1 ":=" $3 }
  | MINUS    expr %prec prec_unary_minus { mkuminus "-" $2 }
  | MINUSDOT expr %prec prec_unary_minus { mkuminus "-." $2 }
  | PLUS     expr %prec prec_unary_plus  { mkuplus "+" $2 }
  | PLUSDOT  expr %prec prec_unary_plus  { mkuplus "+." $2 }

  | simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS expr
                                                     { mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "Array" "set")),
                                                                         ["",$1; "",$4; "",$7])) }
  | ASSERT simple_expr %prec below_SHARP             { mkexp_attrs (Pexp_assert $2) (None, []) }
simple_expr:
  | LIDENT                                           { mkexp(Pexp_ident (mkrhs (Lident $1) 1)) }
  | mod_longident DOT LIDENT                         { mkexp(Pexp_ident (mkrhs (Ldot($1, $3)) 1)) }
  | INT                                              { mkexp(Pexp_constant(Const_int $1)) }
  | STRING                                           { mkexp(Pexp_constant(Const_string (fst $1, snd $1))) }
  | FLOAT                                            { mkexp(Pexp_constant(Const_float $1)) }
  | constr_longident %prec prec_constant_constructor { mkexp(Pexp_construct(mkrhs $1 1, None)) }
  | LPAREN seq_expr RPAREN                           { reloc_exp $2 }
  | simple_expr DOT LPAREN seq_expr RPAREN           { mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "Array" "get")),
                                                       ["",$1; "",$4])) }
simple_labeled_expr_list:
  | labeled_simple_expr                              { [$1] }
  | simple_labeled_expr_list labeled_simple_expr     { $2 :: $1 }
labeled_simple_expr:
  | simple_expr %prec below_SHARP { ("", $1) }
let_bindings:
  | LIDENT fun_binding            { [Vb.mk ~loc:(symbol_rloc()) (mkpatvar $1 1) $2] }
  | pattern EQUAL seq_expr        { [Vb.mk ~loc:(symbol_rloc()) $1 $3] }

fun_binding:
  | EQUAL seq_expr                { $2 }
  | simple_pattern fun_binding    { ghexp(Pexp_fun("", None, $1, $2)) }
expr_comma_list:
  | expr_comma_list COMMA expr    { $3 :: $1 }
  | expr COMMA expr               { [$3; $1] }

/* Patterns */

pattern:
  | simple_pattern                              { $1 }
  | pattern_comma_list  %prec below_COMMA       { mkpat(Ppat_tuple(List.rev $1)) }
simple_pattern:
  | LIDENT %prec below_EQUAL                    { mkpat(Ppat_var (mkrhs $1 1)) }
  | UNDERSCORE                                  { mkpat(Ppat_any) }
  | constr_longident                            { mkpat(Ppat_construct(mkrhs $1 1, None)) }
  | LPAREN pattern RPAREN                       { reloc_pat $2 }

pattern_comma_list:
  | pattern_comma_list COMMA pattern            { $3 :: $1 }
  | pattern COMMA pattern                       { [$3; $1] }

/* Identifiers and long identifiers */

constr_longident:
  | mod_longident       %prec below_DOT         { $1 }
  | LPAREN RPAREN                               { Lident "()" }
  | FALSE                                       { Lident "false" }
  | TRUE                                        { Lident "true" }
mod_longident:
  | UIDENT                                      { Lident $1 }
  | mod_longident DOT UIDENT                    { Ldot($1, $3) }

/* Miscellaneous */

rec_flag:
  |                                             { Nonrecursive }
  | REC                                         { Recursive }

%%
