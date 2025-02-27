// [[arg: %d\n]]

int printf(char const*, ...);

int f(int x) {
    char s[] = {37, 100, 10, 0};
    printf(s, x);
    return x;
}

int main(int, char** argv) {
    int xs[2][2] = {
        // [[print: 1]]
        [0][0] = f(1),
        // [[print: 2]]
        [0][1] = f(2),
        // [[print: 3]]
        [1] = f(3),
        // [[print: 4]]
        [1][1] = f(4),
    };

    // [[print: 1]]
    printf(argv[1], xs[0][0]);
    // [[print: 2]]
    printf(argv[1], xs[0][1]);
    // [[print: 3]]
    printf(argv[1], xs[1][0]);
    // [[print: 4]]
    printf(argv[1], xs[1][1]);
}
