.bytecode 52.0
.source a.java

.class a
.super java.lang.Object

.field public aa I 

.method  <init>()V
	.limit stack 1
	.limit locals 1
	.line 1
		0: 	aload_0
		1: 	invokenonvirtual java/lang/Object/<init>()V
		4: 	return
.end method

.method static public main([Ljava/lang/String;)V
	.limit stack 2
	.limit locals 1
	.line 4
		0: 	getstatic java/lang/System/out Ljava/io/PrintStream;
		3: 	ldc "aatest"
		5: 	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
	.line 5
		8: 	new a$In
		11: 	dup
		12: 	invokenonvirtual a$In/<init>()V
		15: 	pop
	.line 6
		16: 	return
.end method

