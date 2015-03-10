.bytecode 49.0
.source Test_m.java

.class  tests/Test_m
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
	.limit locals 5
	.line 4
		0: 	bipush 10
		2: 	bipush 10
		4: 	multianewarray [[I 2
		8: 	astore_1
	.line 5
		9: 	aload_1
		10: 	iconst_0
		11: 	bipush 10
		13: 	newarray int
		15: 	aastore
	.line 6
		16: 	aload_1
		17: 	iconst_0
		18: 	aaload
		19: 	astore_2
	.line 8
		20: 	aload_2
		21: 	dup
		22: 	astore_3
		23: 	monitorenter
	.line 9
		24: 	getstatic java/lang/System/out Ljava/io/PrintStream;
		27: 	ldc "sync"
		29: 	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
	.line 10
		32: 	aload_3
		33: 	monitorexit
		34: 	goto 44
		37: 	astore 4
		39: 	aload_3
		40: 	monitorexit
		41: 	aload 4
		43: 	athrow
	.line 11
		44: 	return
.end method

