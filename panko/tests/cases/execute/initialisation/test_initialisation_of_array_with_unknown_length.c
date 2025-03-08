// [[arg: %d\n]]
// [[arg: %zu\n]]

int printf(char const*, ...);

int main(int, char** argv) {
    int xs[] = {42, 27, 5};
    // [[print: 42]]
    printf(argv[1], xs[0]);
    // [[print: 27]]
    printf(argv[1], xs[1]);
    // [[print: 5]]
    printf(argv[1], xs[2]);
    // [[print: 12]]
    printf(argv[2], sizeof xs);
    // [[print: 3]]
    printf(argv[2], _Lengthof xs);
}
