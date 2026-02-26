// [[known-bug]]

int printf(char const*, ...);

struct Struct {
    char string[7];
};

void print_struct(struct Struct value) {
    printf("%s\n", value.string);
}

int main() {
    struct Struct value = {"012345"};
    // [[print: 012345]]
    print_struct(value);
}
