int printf(char const*, ...);

struct Large {
    int x;
    char const* string;
    int xs[5];
};

void print_large(struct Large large) {
    printf(
        "struct Large {\n"
        "    int x = %d;\n"
        "    char const* string = \"%s\";\n"
        "    int xs[5] = {%d, %d, %d, %d, %d};\n"
        "}\n",
        large.x,
        large.string,
        large.xs[0],
        large.xs[1],
        large.xs[2],
        large.xs[3],
        large.xs[4]
    );
}

int main() {
    struct Large large = {
        42,
        "hello world",
        {1, 2, 3, 4, 5},
    };
    // [[print: struct Large {]]
    // [[print:     int x = 42;]]
    // [[print:     char const* string = "hello world";]]
    // [[print:     int xs[5] = {1, 2, 3, 4, 5};]]
    // [[print: }]]
    print_large(large);
}
