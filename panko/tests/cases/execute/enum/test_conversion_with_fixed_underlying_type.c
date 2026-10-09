// [[return: 1]]

int printf(char const*, ...);

enum E: short { A, B, C };

int main() {
    enum E x = 0x7fff;
    enum E y = 0x8000;

    // [[print: 32767 -32768]]
    printf("%d %d\n", x, y);

    return y < x;
}
