main1:
	gcc main1.c
	gcc main1.c error.c
	gcc main1.c sml_obstack.c error.c
	gcc main1.c sml_tree.c
	gcc -m32 main1.c	error.c sml_obstack.c sml_tree.c control.c objspace.c sml_obj.c 

	gcc -m32 \
		error.c \
		sml_obstack.c \
		sml_tree.c \
		control.c \
		objspace.c \
		sml_obj.c \
		main1.c

all:
	gcc -m32 \
		error.c \
		sml_obstack.c \
		sml_tree.c \
		control.c \
		objspace.c \
		sml_obj.c \
		main.c \
		smlmain.c

	./a.out

all2:
	gcc -m32 \
		error.c \
		sml_obstack.c \
		sml_tree.c \
		control.c \
		objspace.c \
		sml_obj.c \
		main.c \
		smlmain2.c

	./a.out

all3:
	gcc -O3 -m32 \
		error.c \
		sml_obstack.c \
		sml_tree.c \
		control.c \
		objspace.c \
		sml_obj.c \
		main.c \
		smlmain3.c

	./a.out

