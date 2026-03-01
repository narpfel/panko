int printf(char const*, ...);

struct Struct {
    char string[11];
};

void print_struct(char x, struct Struct value, char y, struct Struct other, char z) {
    printf("%s\n", value.string);
    printf("%s\n", other.string);
    printf("%d %d %d\n", x, y, z);
}

int main() {
    struct Struct value = {"0123456789"};
    struct Struct v2 = {"abcdefghjk"};
    // [[print: 0123456789]]
    // [[print: abcdefghjk]]
    // [[print: 123 42 27]]
    print_struct(123, value, 42, v2, 27);
}
