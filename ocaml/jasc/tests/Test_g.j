.bytecode 49.0
.source Test_g.java

.class  tests/Test_g
.super java.lang.Object

.field  b I 
.field static a I 

.method static <clinit>()V
	.limit stack 1
	.limit locals 0
	.line 11
		0: 	iconst_0
		1: 	putstatic tests/Test_g/a I
		4: 	return
.end method

.method  <init>()V
	.limit stack 2
	.limit locals 1
	.line 2
		0: 	aload_0
		1: 	invokenonvirtual java/lang/Object/<init>()V
	.line 12
		4: 	aload_0
		5: 	iconst_0
		6: 	putfield tests/Test_g/b I
		9: 	return
.end method

.method static public main([Ljava/lang/String;)V
	.limit stack 3
	.limit locals 1
	.line 4
		0: 	getstatic java/lang/System/out Ljava/io/PrintStream;
		3: 	getstatic tests/Test_g/a I
		6: 	invokevirtual java/io/PrintStream/println(I)V
	.line 5
		9: 	getstatic java/lang/System/out Ljava/io/PrintStream;
		12: 	new tests/Test_g
		15: 	dup
		16: 	invokenonvirtual tests/Test_g/<init>()V
		19: 	getfield tests/Test_g/b I
		22: 	invokevirtual java/io/PrintStream/println(I)V
	.line 7
		25: 	getstatic tests/Test_g/a I
		28: 	bipush 10
		30: 	if_icmpge 44
	.line 8
		33: 	getstatic tests/Test_g/a I
		36: 	iconst_1
		37: 	iadd
		38: 	putstatic tests/Test_g/a I
		41: 	goto 25
	.line 10
		44: 	return
.end method

