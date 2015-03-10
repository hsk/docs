.bytecode 49.0
.source Test009.java

.class  tests/Test009
.super java.lang.Object


.method static public main([Ljava/lang/String;)V
	.limit stack 3
	.limit locals 2
	.line 4
		0: 	ldc 100000
		2: 	anewarray java/lang/String
		5: 	astore_1
	.line 5
		6: 	aload_1
		7: 	iconst_0
		8: 	ldc "aaa"
		10: 	aastore
	.line 6
		11: 	aload_1
		12: 	ldc 50000
		14: 	ldc "bbbb"
		16: 	aastore
	.line 7
		17: 	aload_1
		18: 	iconst_1
		19: 	aconst_null
		20: 	aastore
	.line 8
		21: 	getstatic java/lang/System/out Ljava/io/PrintStream;
		24: 	aload_1
		25: 	iconst_0
		26: 	aaload
		27: 	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
	.line 9
		30: 	getstatic java/lang/System/out Ljava/io/PrintStream;
		33: 	aload_1
		34: 	ldc 50000
		36: 	aaload
		37: 	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
	.line 10
		40: 	getstatic java/lang/System/out Ljava/io/PrintStream;
		43: 	aload_1
		44: 	arraylength
		45: 	invokevirtual java/io/PrintStream/println(I)V
	.line 11
		48: 	return
.end method

.method  <init>()V
	.limit stack 1
	.limit locals 1
	.line 2
		0: 	aload_0
		1: 	invokenonvirtual java/lang/Object/<init>()V
		4: 	return
.end method

