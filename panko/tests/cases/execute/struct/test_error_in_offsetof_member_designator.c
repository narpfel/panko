#include <stddef.h>

struct Inner {
    int x;
    int y;
};

struct Outer {
    int xs[5];
    struct Inner inners[10];
};

int main() {
    // [[compile-error: index `20` is out of bounds for array of type `array<int; 5>`]]
    offsetof(struct Outer, xs[20]);
}
