/* TODO Where is this used?  */
/* Test input for DWARF 4 integration tests.
   Compiled with: gcc -gdwarf-4 -O1 -fdebug-types-section
   Produces .debug_loc, .debug_types, and .debug_ranges sections. */
#include <stdio.h>

struct Point {
    int x;
    int y;
};

__attribute__((noinline))
int compute(int a, int b) {
    struct Point p;
    p.x = a;
    p.y = b;
    if (a > 0) {
        p.x += b;
    } else {
        p.y -= a;
    }
    return p.x + p.y;
}

__attribute__((noinline))
int helper(int n) {
    int sum = 0;
    for (int i = 0; i < n; i++) {
        if (i % 2 == 0)
            sum += compute(i, n);
        else
            sum -= compute(n, i);
    }
    return sum;
}

int main() {
    volatile int x = 3;
    int result = helper(x);
    printf("result: %d\n", result);
    return result;
}
