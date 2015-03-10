.bytecode 49.0
.source Test004.java

.class  tests/Test004
.super java.lang.Object


.method static public main([Ljava/lang/String;)V
	.limit stack 2
	.limit locals 1
	.line 5
		0: 	new tests/Test004
		3: 	dup
		4: 	invokenonvirtual tests/Test004/<init>()V
		7: 	invokevirtual tests/Test004/test()V
	.line 6
		10: 	return
.end method

.method public test()V
	.limit stack 2
	.limit locals 1
	.line 9
		0: 	getstatic java/lang/System/out Ljava/io/PrintStream;
		3: 	ldc "test"
		5: 	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
	.line 10
		8: 	return
.end method

.method  <init>()V
	.limit stack 1
	.limit locals 1
	.line 2
		0: 	aload_0
		1: 	invokenonvirtual java/lang/Object/<init>()V
		4: 	return
.end method

