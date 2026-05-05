int printf(char const*, ...);

void f() {
    printf("%zu\n", __panko_gp_offset);
}

void g(int) {
    printf("%zu\n", __panko_gp_offset);
}

struct TwoRegs {
    long x, y;
};

void h(struct TwoRegs) {
    printf("%zu\n", __panko_gp_offset);
}

void all_regs_used(int, int, int, int, int, int, int, int) {
    printf("%zu\n", __panko_gp_offset);
}

int main() {
    // [[print: 0]]
    f();

    // [[print: 8]]
    g(0);

    struct TwoRegs two_regs = {};
    // [[print: 16]]
    h(two_regs);

    // [[print: 48]]
    all_regs_used(1, 2, 3, 4, 5, 6, 7, 8);

    _Generic(__panko_overflow_arg_area, void*: 0);
}
