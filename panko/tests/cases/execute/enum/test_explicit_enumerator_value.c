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
}
