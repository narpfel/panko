// [[known-bug: enumerators have the wrong type during `enum` definition]]

int printf(char const*, ...);

enum E {
    A = 42,
    B = sizeof(A),
    C = sizeof(typeof(B)),
};

int main() {
    // [[print: 42 4 4]]
    printf("%d %d %d\n", A, B, C);
}
