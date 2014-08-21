#import <iostream>
#import <memory>

struct a{
  int n;
  a(int n):n(n){}
  ~a() {
    std::cout << "delete " << n << std::endl;
  }
};

typedef std::shared_ptr<a> A;

A test() {
    A a1 = A(new a(1));
    A a2 = A(new a(2));
    return a1;
}

void test2() {
    A a = test();
}

int main() {
    test2();
    return 0;
}


