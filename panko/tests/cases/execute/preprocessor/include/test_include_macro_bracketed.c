#define LANGLE <
#define RANGLE >
#define MACRO iso646.h
#include LANGLE MACRO RANGLE

int printf(char const*, ...);

int main() {
    // [[print: 1111]]
    printf("%b\n", 0b1001 bitor 0b0110);
}
