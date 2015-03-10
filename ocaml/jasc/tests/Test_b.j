.bytecode 49.0
.source Test_b.java

.class  tests/Test_b
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
	.limit locals 3
	.line 4
		0: 	bipush 10
		2: 	newarray byte
		4: 	astore_1
	.line 5
		5: 	aload_1
		6: 	iconst_0
		7: 	iconst_1
		8: 	bastore
	.line 6
		9: 	getstatic java/lang/System/out Ljava/io/PrintStream;
		12: 	aload_1
		13: 	iconst_0
		14: 	baload
		15: 	invokevirtual java/io/PrintStream/println(I)V
	.line 8
		18: 	iconst_1
		19: 	istore_2
	.line 9
		20: 	iload_2
		21: 	ifne 24
		24: 	iconst_1
		25: 	istore_2
	.line 10
		26: 	iload_2
		27: 	ifeq 34
		30: 	iconst_1
		31: 	goto 35
		34: 	iconst_0
		35: 	istore_2
	.line 11
		36: 	return
.end method

