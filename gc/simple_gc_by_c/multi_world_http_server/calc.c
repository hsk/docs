#include "gc.h"

enum {val_START, val_SIZE, result, A, B, C, val_END};

enum {EInt, EAdd, EMul, ESub, EDiv};


static Object* eint(long i) {
  enum {val_START, val_SIZE, e, val_END};
  ENTER_FRAME_ENUM(val);

	val[e] = gc_alloc_unboxed_array(2);
	val[e]->longs[0] = EInt;
	val[e]->longs[1] = i;
  LEAVE_FRAME(val);
  return val[e];
}

static Object* ebin(long tag, Object* e1, Object* e2) {
  enum {val_START, val_SIZE, e, val_END};
  ENTER_FRAME_ENUM(val);
	val[e] = gc_alloc_record(3);
	val[e]->longs[3] = BIT(1) | BIT(2);
	val[e]->longs[0] = tag;
	val[e]->field[1] = e1;
	val[e]->field[2] = e2;
  LEAVE_FRAME(val);
  return val[e];
}

static Object* eval(Object* e) {
  enum {val_START, val_SIZE, e1, e2, val_END};
  ENTER_FRAME_ENUM(val);
	switch(e->longs[0]) {
	case EInt:
		val[e1] = gc_alloc_long(e->longs[1]);
		break;
	case EAdd:
		val[e1] = eval(e->field[1]);
		val[e2] = eval(e->field[2]);
		val[e1] = gc_alloc_long(val[e1]->longv + val[e2]->longv);
		break;
	case EMul:
		val[e1] = eval(e->field[1]);
		val[e2] = eval(e->field[2]);
		val[e1] = gc_alloc_long(val[e1]->longv * val[e2]->longv);
		break;
	default:
		val[e1] = gc_alloc_long(1);
		break;
	}
  LEAVE_FRAME(val);
  return val[e1];
}

static void model(Object** val) {
	val[result] = str("test2");
	val[A] = ebin(EMul, ebin(EAdd,eint(1),eint(2)), eint(5));
	val[B] = eval(val[A]);
}

static void view(Object** val) {
  printf("HTTP/1.0 200 OK\n");
  printf("text/html\n");
  printf("Cache-Control: max-age=0\n\n");

  printf("<html>\n");
  printf("<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />");
  printf("<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />\n");
  printf("<body>\n");
  printf("%s<br/>", val[result]->chars);
  printf("(1+2)*5=%ld<br/>", val[B]->longv);

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
