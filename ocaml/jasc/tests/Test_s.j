.bytecode 49.0
.source Test_s.java

.class  tests/Test_s
.super java.lang.Object


.method static public main([Ljava/lang/String;)V
	.limit stack 3
	.limit locals 4
	.line 4
		0: 	sipush 1234
		3: 	istore_1
	.line 5
		4: 	iload_1
		5: 	sipush 1234
		8: 	ior
		9: 	istore_1
	.line 7
		10: 	iconst_1
		11: 	newarray short
		13: 	astore_2
	.line 8
		14: 	aload_2
		15: 	iconst_0
		16: 	saload
		17: 	istore_3
	.line 9
		18: 	aload_2
		19: 	iconst_0
		20: 	iload_3
		21: 	sastore
	.line 10
		22: 	return
.end method

.method  <init>()V
	.limit stack 1
	.limit locals 1
	.line 2
		0: 	aload_0
		1: 	invokenonvirtual java/lang/Object/<init>()V
		4: 	return
.end method

