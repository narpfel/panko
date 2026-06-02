int printf(char const*, ...);

int main() {
    static unsigned n = 1u << 31;
    // [[print: 1]]
    printf("%d\n", n == (1u << 31));

    static int a = 0x12345 >> 4;
    // [[print: 0x1234]]
    printf("0x%x\n", a);

    static int b = 0b101010 & 0b111;
    // [[print: 0b10]]
    printf("0b%b\n", b);

    static int c = 0b101010 | 0b111;
    // [[print: 0b101111]]
    printf("0b%b\n", c);

    static int d = 0b101010 ^ 0b111;
    // [[print: 0b101101]]
    printf("0b%b\n", d);

    static long long ll = 123ll - 456;
    // [[print: -333]]
    printf("%lld\n", ll);

    static unsigned long long ull = 123ull + 456;
    // [[print: 579]]
    printf("%llu\n", ull);
}
