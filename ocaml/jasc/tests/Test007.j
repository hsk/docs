.bytecode 52.0
.source Test007.java

.class  tests/Test007
.super java.lang.Object


.method static public main([Ljava/lang/String;)V
	.limit stack 2
	.limit locals 1
	.line 5
		0: 	getstatic java/lang/System/out Ljava/io/PrintStream;
		3: 	invokestatic tests/Test007/a()Ljava/lang/String;
		6: 	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
	.line 6
		9: 	return
.end method

.method static a()Ljava/lang/String;
	.limit stack 1
	.limit locals 0
	.line 9
		0: 	ldc "aaa"
		2: 	areturn
.end method

.method  <init>()V
	.limit stack 1
	.limit locals 1
	.line 2
		0: 	aload_0
		1: 	invokenonvirtual java/lang/Object/<init>()V
		4: 	return
.end method

