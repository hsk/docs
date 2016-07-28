#include "ast.h"
#include <stdio.h>
#include <stdlib.h>

extern Prog* programRoot;

int main() {
	/*
		parse_string(
				"define add1(%eax:int,%ebx:int):int { "
				"  %eax:int = add %eax %ebx "
				"  %ecx:int = set 1 "
				"  %eax:int = add %eax %ecx "
				"  mov %eax "
				"} "
				"%eax:int = set 1 "
				"%ebx:int = set 2 "
				"%eax:int = call add1(%eax:int, %ebx:int) "
				"call min_caml_print_int(%eax:int) "
				);
		delete programRoot;
	*/
	parse_string(
				"define fib(n:int,a:int,b:int):int { "
				"  ifeq n 0 { "
				"    mov a "
				"  } else { "
				"    n1:int = sub n 1 "
				"    ab:int = add a b "
				"    call fib(n1:int, b:int,ab:int) "
				"  } "
				"} "
				"n:int = set 10 "
				"a:int = set 0 "
				"b:int = set 1 "
				"c:int = call fib(n:int,a:int,b:int) "
				"call min_caml_print_int(c:int) "
			);
	auto prog = std::unique_ptr<Prog>(programRoot);
	printf("%s\n", show_prog(prog.get()).c_str());
	prog = simm(std::move(prog));
	printf("%s\n", show_prog(prog.get()).c_str());
	FILE* fp = fopen("a.s", "w");
	prog = regAlloc(std::move(prog));
	printf("%s\n", show_prog(prog.get()).c_str());
	
	emit(fp, prog.get());
	fclose(fp);
	system("gcc -m32 -o a.exe a.s stub.c x86_libmincaml.s");
	system("./a.exe");
	return 0;
}
