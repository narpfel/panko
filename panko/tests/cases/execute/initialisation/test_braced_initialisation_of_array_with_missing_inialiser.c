// [[arg: %d\n]]
int printf(char const*, ...);

int main(int, char** argv) {
    int xs[2][2] = {{1}, {3, 4}};

    // [[print: 1]]
    printf(argv[1], xs[0][0]);
    // [[print: 0]]
    printf(argv[1], xs[0][1]);
    // [[print: 3]]
    printf(argv[1], xs[1][0]);
    // [[print: 4]]
    printf(argv[1], xs[1][1]);
}
