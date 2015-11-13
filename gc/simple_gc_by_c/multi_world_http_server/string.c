#include "gc.h"
#include <string.h>

static Object* str(char* str) {
  long len = strlen(str);
  Object* o = gc_alloc_unboxed_array(len+1);
  strcpy(o->chars, str);
  return o;
}

static Object* str_cat(Object* str1, Object* str2) {
  long len = strlen(str1->chars) + strlen(str2->chars);
  Object* o = gc_alloc_unboxed_array(len+1);
  sprintf(o->chars, "%s%s", str1->chars, str2->chars);
  return o;
}

enum {val_START, val_SIZE, A, B, Str, Str2, val_END};

static void model(Object** val) {
  val[A] = gc_alloc_int(1);
  gc_alloc_int(12);
  val[Str] = str("test data desu.");
  val[Str2] = str_cat(val[Str], str("hoge"));	
}

static void view(Object** val) {
  printf("<html>\n");
  printf("<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />\n");
  printf("<body>\n");
  printf("<h1>string test</h1>");
  printf("data %d<br/> heap_num %ld<br/>\n", val[A]->intv, vm->heap_num);
  printf("Str %s<br/>", val[Str]->chars);
  printf("Str2 %s<br/>", val[Str2]->chars);
  printf("gc collect<br/>");
  gc_collect();
  printf("heap_num %ld<br/>\n", vm->heap_num);
  printf("<hr/>\n");
  printf("<a href=\"index.html\">back</a>\n");
  printf("</body>\n");
  printf("</html>\n");
}

void get_action() {
  ENTER_FRAME_ENUM(val);
  model(val);
  view(val);
  LEAVE_FRAME(val);
}
