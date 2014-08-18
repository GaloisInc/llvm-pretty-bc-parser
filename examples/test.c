#include <stdio.h>

extern int factorial(int);

int main() {
	printf("factorial(%d) = %d\n", 5, factorial(5));

	return 0;
}
