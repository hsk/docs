.bytecode 52.0
.source Test_c.java

.class  tests/Test_c
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
	.limit locals 4
	.line 4
		0: 	bipush 10
		2: 	newarray char
		4: 	astore_1
	.line 6
		5: 	aload_1
		6: 	iconst_0
		7: 	iconst_1
		8: 	castore
	.line 8
		9: 	getstatic java/lang/System/out Ljava/io/PrintStream;
		12: 	aload_1
		13: 	iconst_0
		14: 	caload
		15: 	invokevirtual java/io/PrintStream/println(C)V
	.line 9
		18: 	aconst_null
		19: 	astore_2
	.line 11
		20: 	aload_2
		21: 	checkcast tests/Test_c
		24: 	astore_3
	.line 12
		25: 	return
.end method

