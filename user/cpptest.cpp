#include "stdio.h"

class Foo {
 public:
  Foo(const char* name) {
    this->name = name;
    printf("Foo#ctor, name=%s\n", name);
  }
  virtual ~Foo() {
    printf("Foo#dtor, name=%s\n", name);
  }

  virtual void say() {
    printf("Foo#say\n");
  }

 protected:
  const char* name;
};

class Bar : public Foo {
 public:
  Bar(const char* name) : Foo(name) {
    printf("Bar#ctor, name=%s\n", name);
  }
  virtual ~Bar() {
    printf("Bar#dtor, name=%s\n", name);
  }

  virtual void say() override {
    printf("Bar#say\n");
  }
};

Foo static_instance("static_instance");

int main() {
  Foo* foo = new Bar("local_variable");
  foo->say();
  delete foo;

  return 0;
}
