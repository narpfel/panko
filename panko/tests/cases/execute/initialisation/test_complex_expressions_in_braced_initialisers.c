// [[arg: %d\n]]
int printf(char const*, ...);

int fib(int n) {
    return n < 3 ? 1 : fib(n - 1) + fib(n - 2);
}

int main(int, char** argv) {
    int x = {42};
    int y = {x};
    // [[print: 42]]
    printf(argv[1], y);
    int z = {-x};
    // [[print: -42]]
    printf(argv[1], z);

    int xs[2] = {x, {z}};
    // [[print: 42]]
    printf(argv[1], xs[0]);
    // [[print: -42]]
    printf(argv[1], xs[1]);

    int two = 2;
    int ys[3] = {two, fib(two), fib(two + two + two) - two};
    // [[print: 2]]
    printf(argv[1], ys[0]);
    // [[print: 1]]
    printf(argv[1], ys[1]);
    // [[print: 6]]
    printf(argv[1], ys[2]);
}
