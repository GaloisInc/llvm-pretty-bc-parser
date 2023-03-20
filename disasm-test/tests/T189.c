#include <stdio.h>

void f() {
  // This code has been carefully designed so that the resulting .ll file
  // will contain an explicit function type in a `call` instruction.
  // See #189 for more information.
  int (*p)(const char*, ...) = &printf;
  p("%d\n", 0);
}
