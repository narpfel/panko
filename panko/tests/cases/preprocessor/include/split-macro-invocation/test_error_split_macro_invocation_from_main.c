#define MACRO(a, b) ((a) + (b + 2))

int main() {
    // GCC allows this (with a `-pedantic` warning), clang errors
    return
        MACRO(123
#include "second.h"
        ;
}
