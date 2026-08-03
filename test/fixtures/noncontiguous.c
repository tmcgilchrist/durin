/* Fixture exercising non-contiguous address ranges and location lists.

   Built with clang/gcc `-gdwarf-5 -O2 -ffunction-sections`. The `cold` path is
   split into a separate section, so the compile unit is non-contiguous and gets
   a DW_AT_ranges (a .debug_rnglists list) instead of low_pc/high_pc. Optimising
   the parameters/locals produces DW_AT_location location lists
   (.debug_loclists) whose entries reference .debug_addr indices. */

#include <stdint.h>

__attribute__((noinline, cold)) static int cold_path(int x) {
  return (x * 7) + 3;
}

__attribute__((noinline)) static int hot_path(int x) { return x + 1; }

int compute(int a, int b) {
  int acc = a;
  for (int i = 0; i < b; i++) {
    if (__builtin_expect(i == 0, 0))
      acc = cold_path(acc);
    else
      acc = hot_path(acc);
  }
  return acc;
}

int main(int argc, char **argv) {
  (void)argv;
  return compute(argc, 10) & 0xff;
}
