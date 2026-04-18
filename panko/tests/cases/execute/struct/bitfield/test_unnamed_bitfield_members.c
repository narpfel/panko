int printf(char const*, ...);

struct ZeroLengthBool {
    int x:2;
    bool:0;
    int y:2;
};

struct ZeroLengthChar {
    int x:2;
    char:0;
    int y:2;
};

struct ZeroLengthShort {
    int x:2;
    short:0;
    int y:2;
};

struct ZeroLengthInt {
    int x:2;
    int:0;
    int y:2;
};

struct ZeroLengthLong {
    int x:2;
    long:0;
    int y:2;
};

int main() {
    // [[print: 4]]
    printf("%zu\n", sizeof(struct ZeroLengthBool));
    // [[print: 4]]
    printf("%zu\n", sizeof(struct ZeroLengthChar));
    // [[print: 4]]
    printf("%zu\n", sizeof(struct ZeroLengthShort));
    // [[print: 8]]
    printf("%zu\n", sizeof(struct ZeroLengthInt));
    // [[print: 12]]
    printf("%zu\n", sizeof(struct ZeroLengthLong));
}
