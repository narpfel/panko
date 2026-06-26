// `__panko_gp_offset` could be constexpr evaluable, but probably doesn’t need to

int printf(char const*, ...);

void declare_static(int, int, int) {
    static typeof(sizeof 0) gp_offset = __panko_gp_offset;
    printf("%zu\n", gp_offset);
}

int main() {
    // this could // [[print: 24]]
    declare_static(1, 2, 3);

    static typeof(sizeof 0) gp_offset = __panko_gp_offset;
    // this could // [[print: 0]]
    printf("%zu\n", gp_offset);
}
