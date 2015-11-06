#include "smlsharp.h"
#include <stdio.h>

int main() {
	sml_set_verbose(MSG_DEBUG);
	sml_debug("test exn");


	char* p = sml_exn_name(SML4Bind);
	printf("%p %p %p %p\n", p, &p[0], &p[1], &p[2]);

	printf("%s\n", SML4Bind);
	

	return 0;
}

