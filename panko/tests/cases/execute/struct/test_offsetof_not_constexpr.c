#include <stddef.h>

struct T { int xs[10]; };

int main() {
    int x = 2;
    // TODO: GCC and clang accept this as an extension
    // [[compile-error: not a constant expression]]
    return offsetof(struct T, xs[x]);
}
