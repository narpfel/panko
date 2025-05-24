int printf(char const*, ...);

int main() {
    char a = 'a';
    unsigned char b = 'b';
    signed char c = 'c';
    // [[print: abc]]
    printf("%c%c%c\n", a, b, c);

    unsigned char x = 200;
    signed char y = -5;
    // [[print: 200]]
    printf("%d\n", x);
    // [[print: -5]]
    printf("%d\n", y);
}
