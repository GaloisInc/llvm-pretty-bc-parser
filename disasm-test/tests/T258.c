__attribute__((__noinline__)) void f(int* x) {}

int main(void) {
  int x;
  f(&x);
  return 0;
}
