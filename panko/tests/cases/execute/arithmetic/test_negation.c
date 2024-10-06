// [[arg: %d\n]]
// [[arg: %ld\n]]

int printf(char const*, ...);

int main(int, char** argv) {
    // [[print: -42]]
    printf(*(argv + 1), -42);
    long l = 27;
    // [[print: -27]]
    printf(*(argv + 2), -l);
    unsigned short s = 5;
    // [[print: -5]]
    printf(*(argv + 1), -s);
}
