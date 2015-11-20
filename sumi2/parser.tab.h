/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     NUMBER = 258,
     SYMBOL = 259,
     GT = 260,
     LT = 261,
     GE = 262,
     LE = 263,
     EQ = 264,
     ASSIGN = 265,
     PLUS = 266,
     MINUS = 267,
     MULT = 268,
     DIV = 269,
     LPAREN = 270,
     RPAREN = 271,
     LBRACK = 272,
     RBRACK = 273,
     EOL = 274,
     ERR = 275,
     COMMA = 276,
     SEMI = 277,
     FUN = 278,
     DEF = 279,
     ARROW = 280,
     STRING = 281,
     NEG = 282
   };
#endif
/* Tokens.  */
#define NUMBER 258
#define SYMBOL 259
#define GT 260
#define LT 261
#define GE 262
#define LE 263
#define EQ 264
#define ASSIGN 265
#define PLUS 266
#define MINUS 267
#define MULT 268
#define DIV 269
#define LPAREN 270
#define RPAREN 271
#define LBRACK 272
#define RBRACK 273
#define EOL 274
#define ERR 275
#define COMMA 276
#define SEMI 277
#define FUN 278
#define DEF 279
#define ARROW 280
#define STRING 281
#define NEG 282




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 15 "parser.y"
{
   E   *node;
   int val;
   char* str;
}
/* Line 1529 of yacc.c.  */
#line 109 "parser.tab.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

