int printf(char const*, ...);


int main() {
    static unsigned n = 1u << 31;
    // [[print: 1]]
    printf("%d\n", n == (1u << 31));
}
