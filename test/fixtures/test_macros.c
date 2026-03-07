#include <stdio.h>
#include <stdlib.h>

// Simple macro definitions
#define MAX_SIZE 1024
#define MIN_SIZE 16
#define VERSION "1.0.0"

// Function-like macros
#define SQUARE(x) ((x) * (x))
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define DEBUG_PRINT(msg) printf("DEBUG: %s\n", msg)

// Conditional compilation
#ifdef DEBUG
#define LOG(msg) printf("LOG: %s\n", msg)
#else
#define LOG(msg)
#endif

// Header file with macros
#include "test_macros_header.h"

int main() {
    int size = MAX_SIZE;
    int min = MIN_SIZE;

    printf("Program version: %s\n", VERSION);
    printf("Max size: %d\n", size);
    printf("Min size: %d\n", min);

    int x = 5;
    int y = 3;

    printf("Square of %d is %d\n", x, SQUARE(x));
    printf("Max of %d and %d is %d\n", x, y, MAX(x, y));

    DEBUG_PRINT("This is a debug message");
    LOG("This is a log message");

    // Undefine and redefine a macro
    #undef MAX_SIZE
    #define MAX_SIZE 2048

    printf("New max size: %d\n", MAX_SIZE);

    // Use header macros
    printf("Platform: %s\n", PLATFORM);
    printf("Header constant: %d\n", HEADER_CONSTANT);
    printf("Stringify test: %s\n", STRINGIFY(test_value));
    printf("Outer macro result: %d\n", OUTER_MACRO(10));

    return 0;
}