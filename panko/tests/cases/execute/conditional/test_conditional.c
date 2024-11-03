// [[arg: %d\n]]
// [[return: 2]]

int printf(char const*, ...);

int main(int, char** argv) {
    // [[print: 27]]
    printf(argv[1], 0 ? 42 : 27);
    return 1 ? 2 : 3;
}
