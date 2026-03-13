int printf(char const*, ...);

struct Struct {
    int x;
    int const y;
    int z;
};

struct Struct make_struct() {
    struct Struct value = {1, 2, 3};
    return value;
}

void print_struct(struct Struct value) {
    printf("%d %d %d\n", value.x, value.y, value.z);
}

int main() {
    struct Struct value = make_struct();
    value.x = 42;
    value.z = 27;

    // [[print: 42 2 27]]
    print_struct(value);
}
