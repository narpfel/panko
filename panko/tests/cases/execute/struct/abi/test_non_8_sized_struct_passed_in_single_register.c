// [[known-bug]]

int printf(char const*, ...);

struct Struct {
    int x;
};

void print_struct(struct Struct value) {
    printf("struct Struct { int x = %d; }\n", value.x);
}

int main() {
    struct Struct value = {42};
    // [[print: struct Struct { int x = 42; }]]
    print_struct(value);
}
