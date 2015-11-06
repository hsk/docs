#include "smlsharp.h"
#include <stdio.h>

void test() {
//	sml_fatal(1, "sml_fatal 1\n");
	sml_error(1, "sml_error %d\n", 1);
	sml_warn(1, "sml_warn 1\n");
	sml_notice("sml_notice\n");
//	sml_sysfatal("sml_sysfatal\n");
	sml_syserror("sml_syserror\n");
	sml_syswarn("sml_syswarn\n");
	sml_debug("sml_debug\n");	
}

int main() {
	sml_debug("no print");

	//print_syserror()
	//print_error();


	printf("MSG_DEBUG ---- \n");
	sml_set_verbose(MSG_DEBUG);
	test();

	printf("MSG_NOTICE ---- \n");
	sml_set_verbose(MSG_NOTICE);
	test();

	printf("MSG_WARN ---- \n");
	sml_set_verbose(MSG_WARN);
	test();

	printf("MSG_ERROR ---- \n");
	sml_set_verbose(MSG_ERROR);
	test();

	printf("MSG_FATAL ---- \n");
	sml_set_verbose(MSG_FATAL);
	test();

	return 0;
}

