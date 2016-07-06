#ifndef SYNTAX_H
#define SYNTAX_H

typedef struct syntax_string {
  int len;
  char buf[0];
} *syntax_string;

typedef struct parser_state {
  void *lval;
} parser_state;

typedef enum {
  syntax_DOUBLE,
  syntax_BOOL,
  syntax_PRE,
  syntax_BIN,
  syntax_VAR,
  syntax_IF,
  syntax_LET,
  syntax_NIL,
  syntax_LAMBDA,
} syntax_tag;

#define syntax_HEADER syntax_tag tag;

typedef struct syntax {
  syntax_HEADER;
} syntax;

extern syntax syntax_nil_v;
#define syntax_nil (&syntax_nil_v)

typedef struct {
  syntax_HEADER;
  double value;
} syntax_double;

typedef struct {
  syntax_HEADER;
  int value;
} syntax_bool;

typedef struct {
  syntax_HEADER;
  int op;
  syntax* e;
} syntax_pre;

typedef struct {
  syntax_HEADER;
  int op;
  syntax* l;
  syntax* r;
} syntax_bin;

typedef struct syntax_var {
  syntax_HEADER;
  int len;
  char id[0];
} syntax_var;

typedef struct syntax_if {
  syntax_HEADER;
  syntax* e1;
  syntax* e2;
  syntax* e3;
} syntax_if;

typedef struct syntax_let {
  syntax_HEADER;
  int rec;
  syntax* e1;
  syntax* e2;
  int len;
  char id[0];
} syntax_let;

typedef struct syntax_lambda {
  syntax_HEADER;
  syntax* env;
  syntax* e1;
  int len;
  char id[0];
} syntax_lambda;

extern syntax* syntax_bin_new(syntax*, int, syntax*);
extern syntax* syntax_pre_new(int, syntax*);
extern syntax* syntax_double_new(double);
extern syntax* syntax_bool_new(int);
extern syntax* syntax_if_new(syntax*, syntax*, syntax*);
extern syntax* syntax_let_new(int rec, const char*, syntax*, syntax*);
extern syntax* syntax_var_new(const char*);
extern syntax* syntax_lambda_new(syntax*, const char*, syntax*);

extern void syntax_free(syntax*);
extern void syntax_p(syntax*, int);

#endif
