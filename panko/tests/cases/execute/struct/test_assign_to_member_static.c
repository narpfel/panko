// [[known-bug]]

int memcmp(void const*, void const*, typeof(sizeof 0));
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

struct Inner inner = {1, 2, 3};
struct Outer outer = {
    42,
    {2742, {1, 2, 3}, {4, 5, 6}},
    {11, 25, 36},
    27,
};

int main() {
    int expected[] = {
        42,
        2742,
        1, 2, 3,
        4, 5, 6,
        11, 25, 36,
        27,
    };

    // [[print: 48 48]]
    printf("%zu %zu\n", sizeof outer, sizeof expected);
    // [[print: 0]]
    printf("%d\n", memcmp(&outer, expected, sizeof outer));

    outer.x = 0x1234;
    struct Middle new_middle = {outer.inner.x, outer.inner};
    outer.middle = new_middle;
    outer.middle.inner2.x = 0x2345;
    outer.middle.inner2.y = 0x2346;
    outer.middle.inner2.z = 0x2347;

    int new_expected[] = {
        0x1234,
        11,
        11, 25, 36,
        0x2345, 0x2346, 0x2347,
        11, 25, 36,
        27,
    };
    // [[print: 0]]
    printf("%d\n", memcmp(&outer, new_expected, sizeof outer));
}
