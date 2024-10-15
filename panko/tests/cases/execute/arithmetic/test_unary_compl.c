// [[arg: %d\n]]
// [[arg: %u\n]]
// [[arg: %ld\n]]
// [[arg: %lu\n]]

int printf(char const*, ...);

int main(int, char** argv) {
    int i = 0;
    int compl_0 = ~i;
    // [[print: -1]]
    printf(*(argv + 1), compl_0);
    // [[print: 4294967295]]
    printf(*(argv + 2), ~0u);
    // [[print: -43]]
    printf(*(argv + 3), ~42l);
    // [[print: 18446744073709551573]]
    printf(*(argv + 4), ~42lu);
    long l = ~42u;
    // [[print: 4294967253]]
    printf(*(argv + 3), l);
}
