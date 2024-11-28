// [[arg: %d\n]]
// [[arg: %ld\n]]

int printf(char const*, ...);

int main(int, char** argv) {
    int zero = {};
    // [[print: 0]]
    printf(argv[1], zero);
    int zeros[3] = {};
    // [[print: 0]]
    printf(argv[1], zeros[0]);
    // [[print: 0]]
    printf(argv[1], zeros[1]);
    // [[print: 0]]
    printf(argv[1], zeros[2]);
    int long_zero = {};
    // [[print: 0]]
    printf(argv[2], long_zero);
}
