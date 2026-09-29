int printf(char const*, ...);

enum E { A, B, C, D };

int main() {
    enum E a = A;
    enum E b = B;
    enum E c = (enum E)C;

    // [[print: 0 1 3 6 4]]
    printf("%d %d %d %d %d\n", a, a + B, b + c, D * c, b << c);
}
