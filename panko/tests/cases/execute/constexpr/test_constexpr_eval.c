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

    static char l[] = "hello world";
    // [[print: hello world: 12]]
    printf("%s: %zu\n", l, sizeof l);

    static int m = 42 && 27;
    // [[print: 1]]
    printf("%d\n", m);

    static int o = 0 && 1 / 0;
    // [[print: 0]]
    printf("%d\n", o);

    static int p = false || 123;
    // [[print: 1]]
    printf("%d\n", p);

    static int q = 42 || 1 / 0;
    // [[print: 1]]
    printf("%d\n", q);

    static int r = 0 || false;
    // [[print: 0]]
    printf("%d\n", r);

    static unsigned s = 0 ? 1 / 0u : 42;
    // [[print: 42]]
    printf("%u\n", s);

    static long t = 1234ul ? 5l : 42 / 0l;
    // [[print: 5]]
    printf("%ld\n", t);

    static int u = (false, 42);
    // [[print: 42]]
    printf("%d\n", u);

    static int v = ((void)0, 1234);
    // [[print: 1234]]
    printf("%d\n", v);

    // constexpr eval of pointers to values with static storage duration
    {
        static long a = -20;
        static long* b = &a;
        // [[print: -20]]
        printf("%ld\n", *b);
        ++*b;
        // [[print: -19]]
        printf("%ld\n", a);
    }

    // constexpr eval of pointer arithmetic
    {
        static int xs[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        static int* p1 = 1 + xs;
        static int* p2 = xs + 5 - 2;
        // [[print: 1 3]]
        printf("%d %d\n", *p1, *p2);
        // [[print: 1 3]]
        printf("%td %td\n", p1 - xs, p2 - xs);
    }

    // addressof of member access
    {
        struct T {
            int x;
            int y;
            int z;
        };
        static struct T t = {10, 20, 30};
        static int* p = &t.y;
        // [[print: 10 20 30]]
        printf("%d %d %d\n", p[-1], p[0], p[1]);
        --*p--;
        // [[print: 10 19 30]]
        printf("%d %d %d\n", p[0], p[1], p[2]);

        static int* p2 = &(t).z;
        // [[print: 1]]
        printf("%d\n", p + 2 == p2);
    }

    // bitshift with unsigned rhs
    {
        static int x = 0b11 << 4u;
        // [[print: 0b110000]]
        printf("0b%b\n", x);
    }

    // convert pointer to bool
    {
        static int x;
        static bool is_nonnull = (bool)&x;
        static bool one_past_end_is_nonnull = (bool)(&x + 1);
        // [[print: 1 1]]
        printf("%d %d\n", is_nonnull, one_past_end_is_nonnull);
    }
}
