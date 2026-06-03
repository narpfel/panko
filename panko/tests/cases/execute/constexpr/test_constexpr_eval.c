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

    static int e = 1ll == 0;
    // [[print: 0]]
    printf("%d\n", e);

    static int f = 1 != 0u;
    // [[print: 1]]
    printf("%d\n", f);

    static int g = 1 < 0;
    // [[print: 0]]
    printf("%d\n", g);

    static int h = 0u <= 0;
    // [[print: 1]]
    printf("%d\n", h);

    static int i = 1 > 0;
    // [[print: 1]]
    printf("%d\n", i);

    static int j = 42 >= 42;
    // [[print: 1]]
    printf("%d\n", j);

    static int* k = nullptr;
    // [[print: 0 1]]
    printf("%zu %d\n", (typeof(sizeof 0))k, k == nullptr);
}
