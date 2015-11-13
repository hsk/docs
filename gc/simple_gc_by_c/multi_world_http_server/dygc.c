#include <stdio.h>
#include <dlfcn.h>
#include <string.h>

void invoke_method(char *lib, char *method) {
  void *dl_handle;
  void (*func)();
  char *error;

  dl_handle = dlopen(lib, RTLD_LAZY);
  if (!dl_handle) {
    printf("dlopen error !!! %s\n", dlerror());
    return;
  }

  func = dlsym(dl_handle, method);
  error = dlerror();
  if (error != NULL) {
    printf("dlsym error !!! %s\n", error);
    return;
  }

  (*func)();

  dlclose(dl_handle);
}

int main() {
	gc_init();
    invoke_method("test1.so","method1");
    invoke_method("test.so","test");
    gc_free();
    return 0;
}