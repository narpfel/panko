int printf(char const*, ...);

enum E {
    A = 0x7fff'ffff,
    B,
};

int main() {
    // [[print: 1 1]]
    printf("%d %d\n", A < B, B > 0);
}
