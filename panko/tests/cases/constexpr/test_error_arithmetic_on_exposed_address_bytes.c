#include <stddef.h>

int a;

// okay
size_t b = (size_t)&a;
size_t c = (size_t)(&a + 1);

// GCC and clang allow this, but panko (currently?) does not
size_t d = (size_t)&a + 1;

// invalid
size_t e = ~(size_t)&a;
size_t f = -(size_t)&a;
size_t g = (size_t)&a / 2;
size_t h = 2 * (size_t)&a;

// allowed by GCC
size_t i = (size_t)&a | (size_t)&a;
