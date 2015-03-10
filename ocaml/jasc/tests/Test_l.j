.bytecode 49.0
.source Test_l.java

.class tests/Test_l
.super java.lang.Object


.method static public main([Ljava/lang/String;)V
	.limit stack 4
	.limit locals 13
	.line 4
		0: 	lconst_1
		1: 	lstore_1
	.line 5
		2: 	lload_1
		3: 	l2f
		4: 	fstore_3
	.line 6
		5: 	lload_1
		6: 	l2d
		7: 	dstore 4
	.line 7
		9: 	lload_1
		10: 	l2i
		11: 	istore 6
	.line 9
		13: 	iconst_1
		14: 	newarray long
		16: 	astore 7
	.line 10
		18: 	aload 7
		20: 	iconst_0
		21: 	laload
		22: 	lstore_1
	.line 11
		23: 	aload 7
		25: 	iconst_0
		26: 	lload_1
		27: 	lastore
	.line 13
		28: 	lload_1
		29: 	lconst_1
		30: 	lcmp
		31: 	ifge 38
		34: 	iconst_1
		35: 	goto 39
		38: 	iconst_0
		39: 	istore 8
	.line 14
		41: 	lload_1
		42: 	lconst_1
		43: 	lcmp
		44: 	ifle 51
		47: 	iconst_1
		48: 	goto 52
		51: 	iconst_0
		52: 	istore 8
	.line 16
		54: 	lload_1
		55: 	ldc2_w 2
		58: 	ladd
		59: 	lstore_1
	.line 17
		60: 	lload_1
		61: 	ldc2_w 3
		64: 	ldiv
		65: 	lstore_1
	.line 18
		66: 	lload_1
		67: 	ldc2_w 4
		70: 	lmul
		71: 	lstore_1
	.line 19
		72: 	lload_1
		73: 	lneg
		74: 	lstore_1
	.line 20
		75: 	lload_1
		76: 	ldc2_w 2
		79: 	lrem
		80: 	lstore_1
	.line 21
		81: 	lload_1
		82: 	ldc2_w 2
		85: 	lsub
		86: 	lstore_1
	.line 22
		87: 	lload_1
		88: 	ldc2_w 1234
		91: 	lor
		92: 	lstore_1
	.line 23
		93: 	lload_1
		94: 	ldc2_w 1234
		97: 	land
		98: 	lstore_1
	.line 24
		99: 	lload_1
		100: 	ldc2_w 1234
		103: 	lor
		104: 	lstore_1
	.line 25
		105: 	lload_1
		106: 	iconst_2
		107: 	lshl
		108: 	lstore_1
	.line 26
		109: 	lload_1
		110: 	iconst_2
		111: 	lshr
		112: 	lstore_1
	.line 27
		113: 	lload_1
		114: 	iconst_2
		115: 	lushr
		116: 	lstore_1
	.line 30
		117: 	lload_1
		118: 	l2i
		119: 	i2b
		120: 	istore 9
	.line 31
		122: 	lload_1
		123: 	l2i
		124: 	i2c
		125: 	istore 10
	.line 32
		127: 	lload_1
		128: 	l2i
		129: 	i2s
		130: 	istore 11
	.line 33
		132: 	lload_1
		133: 	l2i
		134: 	istore 12
	.line 36
		136: 	return
.end method

.method  <init>()V
	.limit stack 1
	.limit locals 1
	.line 2
		0: 	aload_0
		1: 	invokenonvirtual java/lang/Object/<init>()V
		4: 	return
.end method

.method static public a()I
	.limit stack 1
	.limit locals 0
	.line 39
		0: 	iconst_1
		1: 	ireturn
.end method

