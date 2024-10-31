// [[arg: %d\n]]

int printf(char const*, ...);

int f();

int main(int, char** argv) {
    // [[print: 0]]
    printf(argv[1], 0 && 0);
    // [[print: 0]]
    printf(argv[1], 0 && 42);
    // [[print: 0]]
    printf(argv[1], 27 && 0);
    // [[print: 1]]
    printf(argv[1], 42 && 27);

    0 && printf(argv[1], 123);
    // [[print: 456]]
    123 && printf(argv[1], 456);
    // [[print: 5]]
    // [[print: 7]]
    printf(argv[1], 5) && printf(argv[1], 7);

    f() && printf(argv[1], 1);
}

int f() {
    return 0;
}
