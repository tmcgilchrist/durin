#include <stdio.h>

__attribute__((noinline))
int add(int a, int b) {
    int result = a + b;
    return result;
}

__attribute__((noinline))
int multiply(int a, int b) {
    int result = a * b;
    return result;
}

int main() {
    int x = 10;
    int y = 20;
    int sum = add(x, y);
    int product = multiply(x, y);
    printf("sum=%d product=%d\n", sum, product);
    return 0;
}
