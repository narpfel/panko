// [[known-bug: non-`int` as nonexplicit underlying type not implemented]]

int printf(char const*, ...);

enum NonInt {
    A = 42,
    B,
    C = 0x0fff'ffff'ffff'ffff,
    D,
};

int main() {
    // [[print: 42 43]]
    printf("%lu %lu\n", A, B);
    // [[print: 0x0fffffffffffffff 0x1000000000000000]]
    printf("0x%016lx 0x%016lx\n", C, D);
}
