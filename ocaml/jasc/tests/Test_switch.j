.bytecode 49.0
.source Test_switch.java

.class  tests/Test_switch
.super java.lang.Object


.method static public main([Ljava/lang/String;)V
	.limit stack 1
	.limit locals 3
	.line 4
		0: 	iconst_5
		1: 	istore_1
	.line 5
		2: 	iconst_1
		3: 	istore_2
	.line 6
		4: 	iload_1
		5: 	lookupswitch 
			1 : 		32
			2 : 		37
			default : 	39
	.line 7
		32: 	iconst_1
		33: 	istore_2
		34: 	goto 39
	.line 8
		37: 	iconst_1
		38: 	istore_2
	.line 11
		39: 	iload_1
		40: 	tableswitch 1
					68
					73
					78
			default : 	80
	.line 12
		68: 	iconst_1
		69: 	istore_2
		70: 	goto 80
	.line 13
		73: 	iconst_1
		74: 	istore_2
		75: 	goto 80
	.line 14
		78: 	iconst_1
		79: 	istore_2
	.line 16
		80: 	iload_1
		81: 	tableswitch 0
					108
					113
					118
			default : 	120
	.line 17
		108: 	iconst_1
		109: 	istore_2
		110: 	goto 120
	.line 18
		113: 	iconst_1
		114: 	istore_2
		115: 	goto 120
	.line 19
		118: 	iconst_1
		119: 	istore_2
	.line 21
		120: 	return
.end method

.method  <init>()V
	.limit stack 1
	.limit locals 1
	.line 2
		0: 	aload_0
		1: 	invokenonvirtual java/lang/Object/<init>()V
		4: 	return
.end method

