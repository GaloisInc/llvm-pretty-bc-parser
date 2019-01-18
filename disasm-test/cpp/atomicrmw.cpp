#include <string>

struct HP {
  std::string x;
  std::string y;
};

int test_me() {
  HP hp;
  hp.x = "";
  return hp.y.length();
}
