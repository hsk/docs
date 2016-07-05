#ifndef EVAL_H
#define EVAL_H

extern syntax* eval(const syntax*, const syntax*);
extern void eval_init();
extern syntax* lookup(syntax*, char*);

#endif