#import <Foundation/Foundation.h>

@interface Fib : NSObject
-(Fib*)initWithInt:(int)n;
-(int)fib;
@end

@implementation Fib {
  int n;
}

-(Fib*)initWithInt:(int)m
{
  self = [super init];
  if (self) {
    n = m;
  }
  return self;
}

-(int)fib
{
  if (n < 2) return 1;
  return [[[Fib alloc]initWithInt:n-2]fib] +
         [[[Fib alloc]initWithInt:n-1]fib];
}
@end

#include <stdio.h>
#include <sys/time.h>
#include <memory.h>

long gett() {
  struct timeval tv;
  gettimeofday (&tv, NULL);
  return (tv.tv_sec) * 1000 + tv.tv_usec / 1000;
}

int fib(int n) {
  if(n < 2) return 1;
  return fib(n - 2) + fib(n - 1);
}

int main() {
  long start;

  start = gett();
  printf("%d\n", fib(40));
  printf("%ld\n", gett() - start);


  start = gett();
  @autoreleasepool {
    Fib *obj1 = [[Fib alloc]initWithInt:40];
    printf("%d\n", [obj1 fib]);
  }
  printf("%ld\n", gett() - start);
  return 0;
}
