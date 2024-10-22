// [[arg: %d\n]]
// [[arg: 0x%x\n]]
// [[arg: 0x%lx\n]]

int printf(char const*, ...);

int main(int, char** argv) {
    int i = 42;
    char one = 1;

    // [[print: 42]]
    printf(argv[1], i >> 0);
    // [[print: 21]]
    printf(argv[1], 42 >> one);
    // [[print: 10]]
    printf(argv[1], i >> 2);
    // [[print: 0]]
    printf(argv[1], i >> 10);

    // [[print: 42]]
    printf(argv[1], i << 0);
    // [[print: 84]]
    printf(argv[1], i << one);
    // [[print: 43008]]
    printf(argv[1], i << 10);

    unsigned int u = 0xffff'ffff;
    // [[print: 0xfffffffe]]
    printf(argv[2], u << one);
    // [[print: 0x7fffffff]]
    printf(argv[2], u >> one);

    int n = u;
    // [[print: 0xfffffffe]]
    printf(argv[2], n << one);
    // [[print: 0xffffffff]]
    printf(argv[2], n >> one);
    // [[print: 0xffffffff]]
    printf(argv[2], n << 0);
    // [[print: 0xffffffff]]
    printf(argv[2], n >> 0);

    long l = 0xa5a5'a5a5'a5a5'a5a5l;
    // [[print: 0xa5a5a5a5a5a50000]]
    printf(argv[3], l << 16);
    // [[print: 0xffffa5a5a5a5a5a5]]
    printf(argv[3], l >> 16);
    // [[print: 0xa5a5a5a5a5a5]]
    printf(argv[3], (unsigned long)l >> 16);
    // [[print: 0xa5a5a5a5a5a5a5a5]]
    printf(argv[3], l << 0);
    // [[print: 0xa5a5a5a5a5a5a5a5]]
    printf(argv[3], l >> 0);
}
