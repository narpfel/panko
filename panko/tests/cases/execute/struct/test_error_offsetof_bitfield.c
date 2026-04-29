#include <stddef.h>

struct T {
    int x:20;
    int y;
};

int main() {
    // invalid
    // [[compile-error: cannot apply `offsetof` to bitfield member `x`]]
    offsetof(struct T, x);
    // but this is okay
    offsetof(struct T, y);
}
