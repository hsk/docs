#ifndef SYNTAX_H
#define SYNTAX_H

typedef struct parser_state {
  void *lval;
} parser_state;

extern parser_state state;

typedef struct syntax_string {
  int len;
  char buf[0];
} *syntax_string;

typedef enum {
  syntax_DOUBLE,
  syntax_BIN,
} syntax_tag;

#define syntax_HEADER syntax_tag tag;

typedef struct syntax {
  syntax_HEADER;
} syntax;

typedef struct {
  syntax_HEADER;
  double value;
} syntax_double;

typedef struct {
  syntax_HEADER;
  int op;
  syntax* l;
  syntax* r;
} syntax_bin;

extern syntax* syntax_bin_new(syntax*, int, syntax*);
extern syntax* syntax_double_new(double);
extern void syntax_free(syntax*);
extern void syntax_pp(syntax*, int);

#endif
