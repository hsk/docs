namespace arc {

	class object {
	private:
		int ref_cnt;

	public:
		explicit object();
		virtual ~object() {}

		object* addRef() {
			ref_cnt++;
			return this;
		}

		void release() {
			ref_cnt--;
			if (ref_cnt == 0) {
				delete this;
			}
		}
	};

	class Pool {
		Pool* next;
		object* t;
		Pool* stack;
	public:

		Pool(object* t=NULL):t(t),next(NULL),stack(NULL) {
			if(t!=NULL)t->addRef();
		}

		object* add(object* t) {
			if(this->t==NULL) {
				t->addRef();
				this->t = t;
				return t;
			}
			Pool* p = new Pool(t);
			p->next = this->next;
			this->next=p;
			return t;
		}

		void release(object* t) {
			if(this->t == t) {
				t->release();
				this->t = NULL;
				return;
			}
			if(this->next != NULL) next->release(t);
		}

		~Pool() {
			if(t) {
				t->release();
			}
			if(next) delete next;
			if(stack) delete stack;
		}

		void push() {
			Pool* p = new Pool();
			p->t = t;
			p->next = next;
			p->stack = stack;
			stack = p;
			t = NULL;
			next = NULL;
		}
		void pop() {
			if(t) {
				t->release();
				t=NULL;
			}
			if(next) delete next;


			t = stack->t;
			next = stack->next;

			Pool* _stack = stack->stack;
			stack->t = NULL;
			stack->next = NULL;
			stack->stack = NULL;
			delete stack;

			stack = _stack;
		}
		template<class T> T* operator()(T* t) { add(t); return t; }
	};


	extern Pool localPool;

	object::object():ref_cnt(0) {
		localPool.add(this);
	}

	template <class T> struct ptr {
		T* o;
		explicit ptr(T* o):o(o) {
			if(o != NULL) {
				o->addRef();
				localPool.release(o);
			}
		}
		virtual ~ptr() {
			if(o) o->release();
		}
		T* operator->() {
			return o;
		}
		T* operator=(T* t) {
			if(o)o->release();
			t->addRef();
			return o = t;
		}
		T* operator()() {
			return o;
		}
	};

	struct LocalPool {
		LocalPool() {
			localPool.push();
		}
		~LocalPool() {
			localPool.pop();
		}
	};

	extern Pool autoPool;
	struct AutoReleasePool {
		AutoReleasePool() {
			autoPool.push();
		}
		~AutoReleasePool() {
			autoPool.pop();
		}
	};

	template <class T> T* ret(T* t) {
		autoPool.add(t);
		return t;
	}

}

#define arc_return(r) return arc::ret(r)
#define arc_case(e,t,e1) if(t e1 = dynamic_cast<t>(e))

#define arc_instance namespace arc {Pool localPool;Pool autoPool;}
