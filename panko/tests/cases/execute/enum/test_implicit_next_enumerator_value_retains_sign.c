int printf(char const*, ...);

enum E {
    A = 0x7fff'ffff,
    B,
    C = 0x8000'0000,
    D = B == C,
    SizeofA = _Generic(A, int: sizeof A),
    SizeofB = _Generic(B, long: sizeof B),
    SizeofC = _Generic(C, unsigned: sizeof C),
};

int main() {
    // [[print: 1 1]]
    printf("%d %d\n", A < B, B > 0);
    // [[print: 1 4 8 4 4]]
    printf("%d %d %d %d %zu\n", D, SizeofA, SizeofB, SizeofC, sizeof(enum E));
}
