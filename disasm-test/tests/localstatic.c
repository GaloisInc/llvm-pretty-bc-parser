#include <stdio.h>

#define NUM 3

int has_local_static(int x) {
    int y;
    static const void *const disptab[NUM] = { &&fn1, &&fn2, &&fn3 };
    y = x;
start:
    goto *disptab[y];
fn1:
    y += 1;
    goto *disptab[y];
fn2:
    y *= 3;
    goto start;
fn3:
    return y;
}

int main(int argc, char** argv) {
    printf("= %d\n", has_local_static(argc));
}
