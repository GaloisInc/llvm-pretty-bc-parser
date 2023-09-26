#include <stdint.h>
#include <stdatomic.h>

atomic_int val = 0x928;

int do_atomic_update(atomic_int newval) {
    int old_val = 0x928;
    return atomic_compare_exchange_weak(&val, &old_val, newval);
}
