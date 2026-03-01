int printf(char const*, ...);

struct Struct {
    long x;
};

void print_struct(struct Struct value) {
    printf("struct Struct { long x = %ld; }\n", value.x);
}

int main() {
    struct Struct value = {42};
    struct Struct other = {27};

    // [[print: struct Struct { long x = 27; }]]
    print_struct(true ? other : value);
    // [[print: struct Struct { long x = 42; }]]
    print_struct(false ? other : value);
}
