.bytecode 49.0
.source Test_while.java

.class tests/Test_while
.super java.lang.Object


.method static public main([Ljava/lang/String;)V
	.limit stack 2
	.limit locals 2
	.line 6
		0: 	iconst_0
		1: 	istore_1
	.line 7
		2: 	iload_1
		3: 	bipush 48
		5: 	if_icmple 11
		8: 	goto 2
	.line 9
		11: 	return
.end method

.method  <init>()V
	.limit stack 1
	.limit locals 1
	.line 2
		0: 	aload_0
		1: 	invokenonvirtual java/lang/Object/<init>()V
		4: 	return
.end method

