int printf(char const *, ...);

enum E {
    A = 123,
    B = A / 2,
    C = B * 2,
    D = B != C,
    E,
};

int main() {
    // [[print: 123 61 122 1 2]]
    printf("%d %d %d %d %d\n", A, B, C, D, E);

    // test that implicitly adding 1 to `INT64_MAX` works
    enum E2: unsigned long {
        F = 0x7fff'ffff'ffff'ffff,
        G,
    };

    // [[print: 0x7fffffffffffffff]]
    printf("0x%lx\n", F);
    // [[print: 0x8000000000000000]]
    printf("0x%lx\n", G);
}
