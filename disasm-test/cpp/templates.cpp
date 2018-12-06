#include "templates.h"

template <typename T>
int nonzero(T a) {
  return a != 0;
}

extern "C" {
  int nonzeroChar(char a) { return nonzero<char>(a); }
  int nonzeroInt(int a) { return nonzero<int>(a); }
}
