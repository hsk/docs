#include <stdio.h>
#include <sys/time.h>
#include <memory.h>
long gett() {
  timeval tv;
  gettimeofday((&tv), NULL);
  return (((tv . tv_sec) * 1000) + ((tv . tv_usec) / 1000));
} 
int fib(int n) {
  if ((n < 2))
    return 1;
  
  return (fib((n - 2)) + fib((n - 1)));
} 
int main() {
  long start = gett();
  printf("%d\n", fib(40));
  printf("%ld\n", (gett() - start));
  return 0;
} 
