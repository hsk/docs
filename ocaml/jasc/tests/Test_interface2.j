.bytecode 49.0
.source Test_interface2.java

.class public tests/Test_interface2
.super java.lang.Object
.implements tests/Test_interface

.field public a I 

.method public test()I
	.limit stack 1
	.limit locals 1
	.line 4
		0: 	bipush 10
		2: 	ireturn
.end method

.method public <init>()V
	.limit stack 2
	.limit locals 1
	.line 2
		0: 	aload_0
		1: 	invokenonvirtual java/lang/Object/<init>()V
	.line 3
		4: 	aload_0
		5: 	iconst_1
		6: 	putfield tests/Test_interface2/a I
		9: 	return
.end method

.method static public main([Ljava/lang/String;)V
	.limit stack 2
	.limit locals 2
	.line 6
		0: 	new tests/Test_interface2
		3: 	dup
		4: 	invokenonvirtual tests/Test_interface2/<init>()V
		7: 	astore_1
	.line 7
		8: 	aload_1
		9: 	invokeinterface tests/Test_interface/test()I 1
		14: 	pop
	.line 8
		15: 	return
.end method

