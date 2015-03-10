.bytecode 49.0
.source Test005.java

.class  tests/Test005
.super java.lang.Object


.method static public main([Ljava/lang/String;)V
	.limit stack 3
	.limit locals 1
	.line 5
		0: 	getstatic java/lang/System/out Ljava/io/PrintStream;
		3: 	new tests/Test005
		6: 	dup
		7: 	invokenonvirtual tests/Test005/<init>()V
		10: 	iconst_3
		11: 	invokevirtual tests/Test005/test(I)I
		14: 	invokevirtual java/io/PrintStream/println(I)V
	.line 6
		17: 	return
.end method

.method public test(I)I
	.limit stack 1
	.limit locals 2
	.line 9
		0: 	iload_1
		1: 	ireturn
.end method

.method  <init>()V
	.limit stack 1
	.limit locals 1
	.line 2
		0: 	aload_0
		1: 	invokenonvirtual java/lang/Object/<init>()V
		4: 	return
.end method

