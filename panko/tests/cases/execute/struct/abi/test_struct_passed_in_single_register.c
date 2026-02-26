// [[known-bug]]

int printf(char const*, ...);

struct Struct {
    int x, y;
};

void print_struct(struct Struct value) {
    printf("%d %d\n", value.x, value.y);
}

int main() {
    struct Struct value = {42, 27};
    // [[print: 42 27]]
    print_struct(value);
}
