#include <stddef.h>

int printf(char const*, ...);

struct Inner {
    int x;
    int y;
};

struct Middle {
    int x;
    struct Inner inner;
};

struct Outer {
    int x;
    struct Middle middles[10];
};

int main() {
    // [[print: 68]]
    printf("%zu\n", offsetof(struct Outer, middles[5].inner.x));

    // [[print: 12]]
    printf("%zu\n", offsetof(struct Outer, middles->inner.y));

    // [[print: 124]]
    printf("%zu\n", offsetof(struct Outer, middles[10]));

    // [[return: 8]]
    return offsetof(struct Middle, inner.y);
}
