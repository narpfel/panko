int printf(char const*, ...);

struct Outer {
    int x[sizeof(struct Inner { int x; })];
    static_assert(sizeof(struct Inner) == sizeof(int));
    int y;
    static_assert(sizeof(struct InStaticAssert { char c; }) == 1);
};

static_assert(sizeof(struct Inner) == sizeof(int));
static_assert(sizeof(struct InStaticAssert) == 1);
static_assert(sizeof(struct Outer) == 5 * sizeof(int));

union U {
    int x;
    static_assert(21 * 2 == 42);
};

int main() {
    struct Inner inner = {123};
    struct InStaticAssert in_static_assert = {27};
    union U u = {456};
    // [[print: 123 27 456]]
    printf("%d %d %d\n", inner.x, in_static_assert.c, u.x);

    struct Outer value = { .x[0] = 1, .x[1] = 2, .x[2] = 4, .x[3] = 8, .y = 42 };
    // [[print: 1 2 4 8 42]]
    printf("%d %d %d %d %d\n", value.x[0], value.x[1], value.x[2], value.x[3], value.y);
}
