// [[arg: %d\n]]
// [[return: 2]]

int printf(char const*, ...);

int main(int, char** argv) {
    // [[print: 42]]
    // [[print: 27]]
    printf(argv[1], 42), printf(argv[1], 27);
    // [[print: 123]]
    0, printf(argv[1], 123);
    // [[print: 3]]
    printf(argv[1], ((0, 42), 1, (2, 3)));
    return 0, 1, 2;
}
