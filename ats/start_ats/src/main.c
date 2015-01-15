// main.c
#include <stdio.h>

extern unsigned int ats_fib(unsigned int);

int main() {
  printf("fib(7) = %u\n", ats_fib(7));
  return 0;
}

