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

struct UnnamedNonzeroLengthBitfield {
    int x:7;
    long:7;
    int y:7;
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

    union {
        struct UnnamedNonzeroLengthBitfield s;
        int as_int;
    } u = { .s.x = 0b0110'1111, .s.y = 0b0111'0111 };
    // [[print: 4]]
    printf("%zu\n", sizeof(struct UnnamedNonzeroLengthBitfield));
    // [[print: 4]]
    printf("%zu\n", _Alignof(struct UnnamedNonzeroLengthBitfield));
    // [[print: 0b00000000000111011100000001101111]]
    printf("0b%032b\n", u.as_int);
}
