#define MACRO(x) "iso646.h"
#include MACRO(42)

int printf(char const*, ...);

int main() {
    // [[print: 1111]]
    printf("%b\n", 0b1001 bitor 0b0110);
}
