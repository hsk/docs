#include <math.h>
#include <stdarg.h>
#include <stdio.h>

extern  void sumf(int nfirst, ...);

int main(int argc, char **argv)
{
  sumf(11);
  return 0;
}
