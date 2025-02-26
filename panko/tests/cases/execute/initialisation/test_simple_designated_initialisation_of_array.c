// [[arg: %d\n]]

int printf(char const*, ...);

int f(int x) {
    char const s[] = {37, 100, 10, 0};
    printf(s, x);
    return x;
}

int main(int, char** argv) {
    int xs[] = {
        // this is not executed, so it does not print `1`
        [0] = f(1),
        // [[print: 2]]
        [42] = f(2),
        // [[print: 3]]
        [27] = f(3),
        // [[print: 4]]
        f(4),
        // [[print: 5]]
        f(5),
        // [[print: 42]]
        [0] = f(42),
    };

    char const* s = argv[1];

    // [[print: 42]]
    printf(s, xs[0]);
    // [[print: 0]]
    printf(s, xs[1]);
    // [[print: 0]]
    printf(s, xs[26]);
    // [[print: 3]]
    printf(s, xs[27]);
    // [[print: 4]]
    printf(s, xs[28]);
    // [[print: 5]]
    printf(s, xs[29]);
    // [[print: 0]]
    printf(s, xs[30]);
    // [[print: 0]]
    printf(s, xs[41]);
    // [[print: 2]]
    printf(s, xs[42]);
}
