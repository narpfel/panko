int printf(char const*, ...);

struct Size4 {
    int x;
};

struct Size4 make_size4(int value) {
    struct Size4 s = {value};
    return s;
}

struct Size7 {
    char string[7];
};

struct Size7 make_size7() {
    struct Size7 value = {"size 7"};
    return value;
}

struct Size8 {
    long x;
};

struct Size8 make_size8() {
    struct Size8 value = {};
    value.x = 123;
    return value;
}

struct Size11 {
    char string[11];
};

struct Size11 make_size11(bool use_numbers) {
    struct Size11 numbers = {"0123456789"};
    struct Size11 letters = {"abcdefghij"};
    return use_numbers ? numbers : letters;
}

struct Size16 {
    long x, y;
};

struct Size16 make_size16(long x, long y) {
    struct Size16 value = {x, y};
    return value;
}

struct Size27 {
    char string[27];
};

struct Size27 make_size27() {
    struct Size27 value = {"abcdefghijklmnopqrstuvwxyz"};
    return value;
}

int main() {
    struct Size4 size4 = make_size4(42);
    // [[print: 42]]
    printf("%d\n", size4.x);

    struct Size7 size7 = make_size7();
    // [[print: size 7]]
    printf("%s\n", size7.string);

    struct Size8 size8 = make_size8();
    // [[print: 123]]
    printf("%ld\n", size8.x);

    struct Size11 numbers = make_size11(true), letters = make_size11(false);
    // [[print: 0123456789]]
    // [[print: abcdefghij]]
    printf("%s\n%s\n", numbers.string, letters.string);

    struct Size16 size16 = make_size16(42, 27);
    // [[print: 42 27]]
    printf("%ld %ld\n", size16.x, size16.y);

    struct Size27 size27 = make_size27();
    // [[print: abcdefghijklmnopqrstuvwxyz]]
    printf("%s\n", size27.string);
}
