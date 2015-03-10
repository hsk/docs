.bytecode 49.0
.source Test_throws.java

.class public tests/Test_throws
.super java.lang.Object


.method public <init>()V
	.limit stack 1
	.limit locals 1
	.line 2
		0: 	aload_0
		1: 	invokenonvirtual java/lang/Object/<init>()V
		4: 	return
.end method

.method static public main([Ljava/lang/String;)V
	.throws java/lang/Exception
	.limit stack 3
	.limit locals 1
	.line 4
		0: 	new java/lang/Exception
		3: 	dup
		4: 	ldc "aaa"
		6: 	invokenonvirtual java/lang/Exception/<init>(Ljava/lang/String;)V
		9: 	athrow
.end method

