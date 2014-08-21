#import <Foundation/Foundation.h>

@interface A : NSObject {
    @private int n;
}

-(id)initWithN:(int)n;
-(void)dealloc;

@end

@implementation A

-(id)initWithN:(int)n1 {
    self = [super init];
    if(!self) return self;
    n = n1;
    return self;
}

-(void)dealloc {
    NSLog(@"dealloc %d", self->n);
}

@end

A* test() {
    A *a1 = [[A alloc]initWithN:1];

    // これはスコープを抜けると消える。
    A *a2 = [[A alloc]initWithN:2];
    
    return a1;
}

void test2() {
    // この*aはオートリリースプールに含まれる。
    A *a = test();
}

int main() {
    @autoreleasepool {
        test2();
    }
    return 0;
}

