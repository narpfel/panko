// [[arg: %d\n]]

int printf(char const*, ...);

int f(int x) {
    char const s[] = {37, 100, 10, 0};
    printf(s, x);
    return x;
}

int main(int, char** argv) {
    // [0]
    // [[print: 42]]
    // [27]
    // [[print: 3]]
    // [28]
    // [[print: 4]]
    // [29]
    // [[print: 5]]
    // [42]
    // [[print: 2]]
    int xs[] = {
        // this is not executed, so it does not print `1`
        [0] = f(1),
        [42] = f(2),
        [27] = f(3),
        f(4),
        f(5),
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

    // [[print: 200]]
    // [[print: 100]]
    // [[print: 400]]
    int ys[] = {
        [sizeof(short)] = f(100),
        [sizeof(char)] = f(200),
        [sizeof(int)] = f(400),
    };
    // [[print: {0, 200, 100, 0, 400}]]
    printf("{%d, %d, %d, %d, %d}\n", ys[0], ys[1], ys[2], ys[3], ys[4]);
}
