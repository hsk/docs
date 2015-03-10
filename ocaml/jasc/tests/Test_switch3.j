; lookup length 19  calclen 19 array length 1
; table switch length 28 calclen 28 array length 3 padding_size 4
.bytecode 48.0
.source Test_switch3.java

.class  tests/Test_switch3
.super java.lang.Object


.method static public main([Ljava/lang/String;)V
	.limit stack 1
	.limit locals 1
	.line 6
		0: 	iconst_1
		1: 	lookupswitch 
			1 : 		20
			default : 	23
	.line 7
		20: 	goto 61
	.line 9
		23: 	iconst_1
		24: 	tableswitch 1
					52
					55
					58
			default : 	61
	.line 10
		52: 	goto 61
	.line 11
		55: 	goto 61
	.line 12
		58: 	goto 61
	.line 15
		61: 	return
.end method

.method  <init>()V
	.limit stack 1
	.limit locals 1
	.line 3
		0: 	aload_0
		1: 	invokenonvirtual java/lang/Object/<init>()V
		4: 	return
.end method

