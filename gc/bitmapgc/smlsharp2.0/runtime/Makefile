main1:
	gcc main1.c
	gcc main1.c error.c
	gcc main1.c obstack.c error.c
	gcc main1.c splay.c
	gcc main1.c -m32 error.c exn.c obstack.c splay.c heap_bitmap.c control.c objspace.c object.c -lgmp 

	gcc -m32 -lgmp \
		error.c \
		exn.c \
		obstack.c \
		splay.c \
		heap_bitmap.c \
		control.c \
		objspace.c \
		object.c \
		main1.c

	gcc -m32 -lgmp \
		error.c \
		exn.c \
		obstack.c \
		splay.c \
		heap_bitmap.c \
		control.c \
		objspace.c \
		object.c \
		init.c \
		main1.c

	gcc -m32 -lgmp \
		error.c \
		exn.c \
		obstack.c \
		splay.c \
		heap_bitmap.c \
		control.c \
		objspace.c \
		object.c \
		init.c \
		prim.c \
		main1.c

all:
	gcc -DEBUG -m32 -lgmp \
		error.c \
		exn.c \
		obstack.c \
		splay.c \
		heap_bitmap.c \
		control.c \
		objspace.c \
		object.c \
		init.c \
		prim.c \
		main.c \
		smlmain.c
	./a.out

