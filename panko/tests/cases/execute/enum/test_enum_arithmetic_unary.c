int printf(char const*, ...);

enum E { A, B, C };

int main() {
    enum E a = A;
    enum E b = B;
    enum E c = (enum E)C;

    // [[print: 0 1 -1 0xfffffffd 1 0]]
    printf("%d %d %d 0x%08x %d %d\n", +a, +b, -b, ~c, !a, !b);
}
