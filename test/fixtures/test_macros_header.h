#ifndef TEST_MACROS_HEADER_H
#define TEST_MACROS_HEADER_H

// Header macros
#define HEADER_CONSTANT 42
#define STRINGIFY(x) #x
#define CONCAT(a, b) a ## b

// Platform-specific macros
#ifdef __APPLE__
#define PLATFORM "macOS"
#elif defined(__linux__)
#define PLATFORM "Linux"
#else
#define PLATFORM "Unknown"
#endif

// Nested macros
#define OUTER_MACRO(x) INNER_MACRO(x)
#define INNER_MACRO(x) ((x) + HEADER_CONSTANT)

#endif // TEST_MACROS_HEADER_H