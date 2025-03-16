int printf(char const*, ...);

int main() {
    int xss[20][30] = {};

    xss[2][3] = 42;
    // [[print: 42]]
    printf("%d\n", xss[2][3]);

    // [[print: 2400]]
    printf("%zu\n", sizeof xss);
    // [[print: 120]]
    printf("%zu\n", sizeof xss[0]);
    // [[print: 20]]
    printf("%zu\n", _Lengthof xss);
    // [[print: 30]]
    printf("%zu\n", _Lengthof xss[0]);
}
