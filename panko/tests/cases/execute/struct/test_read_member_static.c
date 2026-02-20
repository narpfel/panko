// [[known-bug]]

int printf(char const*, ...);

struct Inner {
    int x;
    int y;
    int z;
};

struct Middle {
    int y;
    struct Inner inner;
    struct Inner inner2;
};

struct Outer {
    int x;
    struct Middle middle;
    struct Inner inner;
    int y;
};

void print_inner(struct Inner* p) {
    printf("Inner { .x = %d, .y = %d, .z = %d }\n", p->x, p->y, p->z);
}

struct Inner inner = {1, 2, 3};
struct Outer outer = {
    42,
    {2742, {1, 2, 3}, {4, 5, 6}},
    {11, 25, 36},
    27,
};

int main() {
    // [[print: Inner { .x = 1, .y = 2, .z = 3 }]]
    print_inner(&inner);
    // [[print: Inner { .x = 4, .y = 5, .z = 6 }]]
    print_inner(&outer.middle.inner2);

    struct Inner inner_read_from_struct = outer.middle.inner2;
    // [[print: Inner { .x = 4, .y = 5, .z = 6 }]]
    print_inner(&inner_read_from_struct);

    // [[print: Inner { .x = 11, .y = 25, .z = 36 }]]
    print_inner(&outer.inner);

    // [[print: 27 2742 6]]
    printf("%d %d %d\n", outer.y, outer.middle.y, outer.middle.inner2.z);

    // [[print: 16]]
    printf("%td\n", (char*)&outer.middle.inner2.y - (char*)&outer.middle.inner);
}
