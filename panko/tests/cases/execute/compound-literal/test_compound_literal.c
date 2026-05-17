int printf(char const*, ...);

int print(int x) {
    printf("%d ", x);
    return x;
}

int* static_compound_literal() {
    return &(static int){};
}

int main() {
    // [[print: 10 5 15]]
    printf("%d\n", (int){(int){print(10)} + (int){print(5)}});

    int* p = static_compound_literal();
    int* q = static_compound_literal();

    *p = 42;
    // [[print: 42 1]]
    printf("%d %d\n", *q, p == q);

    // check that compound literals don’t overlap other local variables
    struct T {
        int x;
        int y;
    };
    int i1 = {};
    struct T* ptr_to_local = &(struct T){.x = 123, .y = 456};
    int i2 = {};
    i1 = 789;
    i2 = 987;
    // [[print: 123 456]]
    printf("%d %d\n", ptr_to_local->x, ptr_to_local->y);
}
