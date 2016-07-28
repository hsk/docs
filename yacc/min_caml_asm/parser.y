%{
#include "ast.h"
Prog *programRoot;
extern int yylex();
extern void yyerror(const char* s, ...);
%}
%union {
  int integer;
  Prog *prog;
  std::vector<std::unique_ptr<Fundef>>* fundefs;
  Fundef* fundef;
  E* term;
  Exp* exp;
  std::string* string;
  svec_t* ids;
  Id_or_imm* imm;
  std::vector<UT>* tys;
  T* ty;
}
%token <integer> INT
%token <string> IDENT
%token LPAREN RPAREN LBRACE RBRACE
%token LET CORON EQ COMMA COLON
%token NOP SET SETL 
%token  MOV  NEG  ADD  SUB            LD  ST
%token UNIT BOOL T_INT ARROW
%token IFEQ IFLE IFGE ELSE
%token DEFINE
%token CALL SAVE RESTORE
%type <prog> program
%type <fundefs> fundefs
%type <fundef> fundef
%type <term> term
%type <exp> exp
%type <ids> args
%type <string> ident;
%type <imm> id_or_imm;
%type <tys> tys;
%type <string> id_ty;
%type <ty> ty;
%nonassoc error

%start program

%%

program     : fundefs term                      { programRoot = new Prog(std::move(*$1), UE($2)); delete $1; }
fundefs     :                                   { $$ = new std::vector<std::unique_ptr<Fundef>>(); }
            | fundefs fundef                    { $$ = $1; $$->push_back(std::unique_ptr<Fundef>($2)); }
fundef      : DEFINE ident LPAREN args RPAREN COLON ty LBRACE term RBRACE
                                                { $$ = new Fundef(*UStr($2), *$4, UE($9), UT($7)); delete $4; }
args        :                                   { $$ = new svec_t(); }
            | id_ty                             { $$ = new svec_t(); $$->push_back(*UStr($1)); }
            | args COMMA id_ty                  { $$ = $1; $$->push_back(*UStr($3)); }
id_ty       : ident COLON ty                    { $$ = $1; delete $3; }
ty          : UNIT                              { $$ = new Unit(); }
            | BOOL                              { $$ = new Bool(); }
            | T_INT                             { $$ = new Int();  }
            | LPAREN tys RPAREN ARROW ty        { $$ = new Fun(std::move(*$2), UT($5)); delete $2; }
tys         :                                   { $$ = new std::vector<UT>(); }
            | tys COMMA ty                      { $$ = $1; $$->push_back(UT($3)); } 
id_or_imm   : ident                             { $$ = new V(*UStr($1)); }
            | INT                               { $$ = new C($1); }
term        : exp                               { $$ = new Ans(UExp($1)); }
            | ident COLON ty EQ exp term        { $$ = new Let(*UStr($1), UT($3), UExp($5), UE($6)); }
            | exp term                          { $$ = seq(UExp($1), UE($2)); }
exp         : NOP                               { $$ = new Nop(); }
            | SET INT                           { $$ = new Set($2); }
            | SETL ident                        { $$ = new SetL(*UStr($2)); }
            | MOV ident                         { $$ = new Mov(*UStr($2)); }
            | NEG ident                         { $$ = new Neg(*UStr($2)); }
            | ADD ident id_or_imm               { $$ = new Add(*UStr($2), UId_or_imm($3)); }
            | SUB ident id_or_imm               { $$ = new Sub(*UStr($2), UId_or_imm($3)); }
            | LD ident id_or_imm INT            { $$ = new Ld(*UStr($2), UId_or_imm($3), $4); }
            | ST ident ident id_or_imm INT      { $$ = new St(*UStr($2), *UStr($3), UId_or_imm($4), $5); }
            | IFEQ ident id_or_imm LBRACE term RBRACE ELSE LBRACE term RBRACE
                                                { $$ = new IfEq(*UStr($2), UId_or_imm($3), UE($5), UE($9)); }
            | IFLE ident id_or_imm LBRACE term RBRACE ELSE LBRACE term RBRACE
                                                { $$ = new IfLE(*UStr($2), UId_or_imm($3), UE($5), UE($9)); }
            | IFGE ident id_or_imm LBRACE term RBRACE ELSE LBRACE term RBRACE
                                                { $$ = new IfGE(*UStr($2), UId_or_imm($3), UE($5), UE($9)); }
            | CALL ident LPAREN args RPAREN     { $$ = new Call(*UStr($2), *$4); delete $4; }
            | SAVE ident ident                  { $$ = new Save(*UStr($2), *UStr($3)); }
            | RESTORE ident                     { $$ = new Restore(*UStr($2)); }
ident       : IDENT                             { $$ = $1; }
