int printf(char const*, ...);

enum NonInt {
    A = 42,
    B,
    C = 0xefff'ffff'ffff'ffff,
    D,
};

int main() {
    // [[print: 42 43]]
    printf("%lu %lu\n", A, B);
    // [[print: 0xefffffffffffffff 0xf000000000000000]]
    printf("0x%016lx 0x%016lx\n", C, D);
}
