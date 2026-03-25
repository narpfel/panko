int printf(char const*, ...);
int memcmp(void const*, void const*, typeof(sizeof 0));

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

struct ContainsUnion {
    int x;
    union StructInt struct_int;
    int y;
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

    struct ContainsUnion contains_union = {2, 0xffff'ffff, 0xffff'fffe, 3};
    // [[print: 2 3]]
    printf("%d %d\n", contains_union.x, contains_union.y);
    // [[print: 1]]
    printf("%d\n", (long)contains_union.struct_int.t.x == 0xffff'ffffLL);
    // [[print: 1]]
    printf("%d\n", (long)contains_union.struct_int.t.y == 0xffff'fffeLL);
    // [[print: 1]]
    printf("%d\n", (long)contains_union.struct_int.x == -1LL);

    struct ContainsUnion contains_union_2 = {2, {{0xffff'ffff, 0xffff'fffe}}, 3};
    // [[print: 0]]
    printf("%d\n", memcmp(&contains_union, &contains_union_2, sizeof contains_union));
}
