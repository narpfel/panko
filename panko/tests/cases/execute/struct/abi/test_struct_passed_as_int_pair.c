// [[known-bug]]

int printf(char const*, ...);

struct Struct {
    char string[11];
};

void print_struct(struct Struct value) {
    printf("%s\n", value.string);
}

int main() {
    struct Struct value = {"0123456789"};
    // [[print: 0123456789]]
    print_struct(value);
}
