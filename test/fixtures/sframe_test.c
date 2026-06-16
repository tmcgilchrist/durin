/* Fixture for SFrame integration tests.
   Compiled with: gcc -O0 -Wa,--gsframe sframe_test.c -o sframe_test_linux
   Each function produces at least one FDE in the .sframe section. */

#include <stdio.h>

static int leaf(int x) { return x + 1; }

static int branch(int x) {
  if (x > 0) return leaf(x);
  return -leaf(-x);
}

int main(void) {
  int n = 0;
  for (int i = -3; i <= 3; i++) n += branch(i);
  printf("%d\n", n);
  return 0;
}
