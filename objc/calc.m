#import <Foundation/Foundation.h>

@interface Exp : NSObject
//-(void)dealloc;
@end

@implementation Exp
//-(void)dealloc {
//    NSLog(@"dealloc exp");
//}
@end


@interface EInt : Exp {
	int i;
}
-(id)initWithInt:(int)i;
-(int)getInt;
-(void)dealloc;
@end

@implementation EInt
-(id)initWithInt:(int)i_ {
	self = [super init];
	if (!self) return self;
	self->i = i_;
	return self;
}
-(int)getInt {
	return self->i;
}
-(void)dealloc {
    NSLog(@"dealloc EInt");
}
@end

@interface EAdd : Exp {
	Exp* l;
	Exp* r;
}
-(Exp*)getL;
-(Exp*)getR;
-(id)initWithL:(Exp*)l R:(Exp*)r;
-(void)dealloc;
@end

@implementation EAdd
-(id)initWithL:(Exp*)l_ R:(Exp*)r_ {
	self = [super init];
	if (!self) return self;
	self->l = l_;
	self->r = r_;
	return self;
}
-(Exp*)getL {
	return self->l;
}
-(Exp*)getR {
	return self->r;
}
-(void)dealloc {
    NSLog(@"dealloc EAdd");
}
@end

@interface ESub : Exp {
	Exp* l;
	Exp* r;
}
-(Exp*)getL;
-(Exp*)getR;
-(id)initWithL:(Exp*)l R:(Exp*)r;
-(void)dealloc;
@end

@implementation ESub
-(id)initWithL:(Exp*)l_ R:(Exp*)r_ {
	self = [super init];
	if (!self) return self;
	self->l = l_;
	self->r = r_;
	return self;
}
-(Exp*)getL {
	return self->l;
}
-(Exp*)getR {
	return self->r;
}
-(void)dealloc {
    NSLog(@"dealloc ESub");
}
@end

int eval(Exp* exp) {
	if ([exp class] == [EInt class]) {
		return [(EInt*) exp getInt];
	}
	if ([exp class] == [EAdd class]) {
		EAdd* i = (EAdd*) exp;
		return eval([i getL]) + eval([i getR]);
	}
	if ([exp class] == [EAdd class]) {
		ESub* i = (ESub*) exp;
		return eval([i getL]) - eval([i getR]);
	}
	return 0;
}

int main() {
	@autoreleasepool {
		EInt* i = [[EInt alloc] initWithInt: 1];
		EAdd* add = [[EAdd alloc] initWithL:i R:[[EInt alloc] initWithInt: 2] ];
		ESub* sub = [[ESub alloc] initWithL:add R:[[EInt alloc] initWithInt: 200] ];
		NSLog(@"result=%d", eval(sub));
	    return 0;
	}
}
