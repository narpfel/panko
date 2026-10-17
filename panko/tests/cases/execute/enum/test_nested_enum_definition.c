int printf(char const*, ...);

enum Outer {
    A = 42,
    B = A + 5,
    C = sizeof(enum Inner { X = B + 2, Y = A + X }),
};

int main() {
    // [[print: 42 47 4 49 91]]
    printf("%d %d %d %d %d\n", A, B, C, X, Y);
}
