.bytecode 49.0
.source Test010.java

.class tests/Test010
.super java.lang.Object


.method  <init>()V
	.limit stack 1
	.limit locals 1
	.line 2
		0: 	aload_0
		1: 	invokenonvirtual java/lang/Object/<init>()V
		4: 	return
.end method

.method static public main([Ljava/lang/String;)V
	.limit stack 3
	.limit locals 8
	.line 5
		0: 	iconst_0
		1: 	istore_1
	.line 6
		2: 	iconst_1
		3: 	istore_2
	.line 7
		4: 	iconst_2
		5: 	istore_3
	.line 8
		6: 	iconst_3
		7: 	istore 4
	.line 9
		9: 	iconst_4
		10: 	istore 5
	.line 10
		12: 	iconst_5
		13: 	istore 6
	.line 11
		15: 	bipush 7
		17: 	istore 7
	.line 12
		19: 	iload_1
		20: 	iconst_1
		21: 	if_icmpne 69
		24: 	iload_2
		25: 	ifeq 69
		28: 	iload_3
		29: 	ifle 69
		32: 	iload 4
		34: 	iflt 69
		37: 	iload 5
		39: 	ifge 69
		42: 	iload 6
		44: 	ifgt 69
		47: 	iload 7
		49: 	iconst_1
		50: 	if_icmpne 69
		53: 	iload 7
		55: 	iconst_1
		56: 	if_icmpeq 69
	.line 13
		59: 	new java/lang/Exception
		62: 	dup
		63: 	ldc "error"
		65: 	invokenonvirtual java/lang/Exception/<init>(Ljava/lang/String;)V
		68: 	athrow
	.line 15
		69: 	goto 73
	.line 14
		72: 	astore_1
	.line 16
		73: 	return
.end method

