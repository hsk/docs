.bytecode 49.0
.source Test_f.java

.class  tests/Test_f
.super java.lang.Object


.method static public main([Ljava/lang/String;)V
	.limit stack 3
	.limit locals 9
	.line 4
		0: 	dconst_1
		1: 	dstore_1
	.line 5
		2: 	dload_1
		3: 	d2f
		4: 	fstore_3
	.line 6
		5: 	dload_1
		6: 	d2i
		7: 	istore 4
	.line 7
		9: 	dload_1
		10: 	d2l
		11: 	lstore 5
	.line 9
		13: 	fload 3
		14: 	f2i
		15: 	istore 4
	.line 10
		17: 	fload 3
		18: 	f2l
		19: 	lstore 5
	.line 11
		21: 	fload 3
		22: 	f2d
		23: 	dstore_1
	.line 13
		24: 	iload 4
		26: 	i2f
		27: 	fstore_3
	.line 14
		28: 	iload 4
		30: 	i2d
		31: 	dstore_1
	.line 15
		32: 	iload 4
		34: 	i2l
		35: 	lstore 5
	.line 17
		37: 	lload 5
		39: 	l2f
		40: 	fstore_3
	.line 18
		41: 	lload 5
		43: 	l2d
		44: 	dstore_1
	.line 19
		45: 	lload 5
		47: 	l2i
		48: 	istore 4
	.line 20
		50: 	iconst_1
		51: 	newarray float
		53: 	astore 7
	.line 21
		55: 	aload 7
		57: 	iconst_0
		58: 	faload
		59: 	fstore_3
	.line 22
		60: 	aload 7
		62: 	iconst_0
		63: 	fload 3
		64: 	fastore
	.line 23
		65: 	fload 3
		66: 	fconst_1
		67: 	fcmpg
		68: 	ifge 75
		71: 	iconst_1
		72: 	goto 76
		75: 	iconst_0
		76: 	istore 8
	.line 24
		78: 	fload 3
		79: 	fconst_1
		80: 	fcmpl
		81: 	ifle 88
		84: 	iconst_1
		85: 	goto 89
		88: 	iconst_0
		89: 	istore 8
	.line 26
		91: 	fload 3
		92: 	fconst_2
		93: 	fadd
		94: 	fstore_3
	.line 27
		95: 	fload 3
		96: 	ldc 3.
		98: 	fdiv
		99: 	fstore_3
	.line 28
		100: 	fload 3
		101: 	ldc 4.
		103: 	fmult
		104: 	fstore_3
	.line 29
		105: 	fload 3
		106: 	fneg
		107: 	fstore_3
	.line 30
		108: 	fload 3
		109: 	fconst_2
		110: 	frem
		111: 	fstore_3
	.line 31
		112: 	fload 3
		113: 	fconst_2
		114: 	fsub
		115: 	fstore_3
	.line 33
		116: 	return
.end method

.method  <init>()V
	.limit stack 1
	.limit locals 1
	.line 2
		0: 	aload_0
		1: 	invokenonvirtual java/lang/Object/<init>()V
		4: 	return
.end method

.method static public a()F
	.limit stack 1
	.limit locals 0
	.line 35
		0: 	ldc 1.10000002384
		2: 	freturn
.end method

