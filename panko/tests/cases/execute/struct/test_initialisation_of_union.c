int printf(char const*, ...);

union IntInt {
    int i;
    unsigned u;
};

struct T {
    unsigned x;
    unsigned y;
};

union StructInt {
    struct T t;
    int x;
};

int main() {
    union IntInt int_int = {-1};
    // [[print: 1]]
    printf("%d\n", (long)int_int.i == -1LL);
    // [[print: 1]]
    printf("%d\n", (long)int_int.u == 0xffff'ffffLL);

    union StructInt struct_int = {0xffff'ffff, 42};
    // [[print: 1]]
    printf("%d\n", (long)struct_int.t.x == 0xffff'ffffLL);
    // [[print: 1]]
    printf("%d\n", (long)struct_int.t.y == 42LL);
    // [[print: 1]]
    printf("%d\n", (long)struct_int.x == -1LL);
}
