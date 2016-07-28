#include "ast.h"
#include <stdio.h>
#include <stdlib.h>

extern Prog* programRoot;

int main(int argn, char** argv) {

	if(argn != 2) {
		printf("usage mincamlasm filename without \".mls\"...\n");
	}
	auto name = std::string(argv[1]);

	FILE* fp = fopen((name+".mls").c_str(), "r");
	parse_input(fp);
	fclose(fp);

	auto prog = std::unique_ptr<Prog>(programRoot);
	printf("%s\n", show_prog(prog.get()).c_str());
	prog = simm(std::move(prog));
	printf("%s\n", show_prog(prog.get()).c_str());
	fp = fopen((name+".s").c_str(), "w");
	prog = regAlloc(std::move(prog));
	printf("%s\n", show_prog(prog.get()).c_str());
	
	emit(fp, prog.get());
	fclose(fp);
	system((std::string("clang -m32 -o ")+name+".exe "+name+".s stub.c x86_libmincaml.s").c_str());
	system((std::string("./")+name+".exe").c_str());
	return 0;
}
