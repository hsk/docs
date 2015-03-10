.bytecode 52.0
.source Test003.java

.class  tests/Test003
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
	.limit stack 2
	.limit locals 1
	.line 4
		0: 	getstatic java/lang/System/out Ljava/io/PrintStream;
		3: 	ldc "test"
		5: 	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
	.line 5
		8: 	return
.end method

