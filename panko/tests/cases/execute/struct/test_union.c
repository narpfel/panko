int printf(char const*, ...);

struct T {
    unsigned x;
};

union U {
    int x;
    struct T t;
};

int main() {
    union U u;
    u.x = -1;
    // [[print: 1]]
    printf("%d\n", (long)u.t.x == 0xffff'ffffLL);
    // [[print: 4]]
    printf("%zu\n", sizeof(union U));
    ++u.t.x;
    return u.x;
}
