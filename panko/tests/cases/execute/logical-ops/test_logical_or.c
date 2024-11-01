// [[arg: %d\n]]

int printf(char const*, ...);

int f();
int g();

int main(int, char** argv) {
    // [[print: 0]]
    printf(argv[1], 0 || 0);
    // [[print: 1]]
    printf(argv[1], 0 || 42);
    // [[print: 1]]
    printf(argv[1], 27 || 0);
    // [[print: 1]]
    printf(argv[1], 42 || 27);

    // [[print: 123]]
    0 || printf(argv[1], 123);
    123 || printf(argv[1], 456);
    // [[print: 5]]
    printf(argv[1], 5) || printf(argv[1], 7);

    // [[print: 1]]
    f() || printf(argv[1], 1);

    g() || printf(argv[1], 2);
}

int f() {
    return 0;
}

int g() {
    return 2;
}
