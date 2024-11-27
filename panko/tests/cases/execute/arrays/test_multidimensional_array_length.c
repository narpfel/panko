// [[return: 69]]
// [[arg: %zu\n]]

int printf(char const*, ...);

int main(int, char** argv) {
    // [[print: 100]]
    printf(argv[1], _Lengthof(long*[100][400]));
    int xs[42][27];
    // [[print: 42]]
    printf(argv[1], _Lengthof xs);
    // [[print: 27]]
    printf(argv[1], _Lengthof *xs);
    int (*ys)[42][27];
    return _Lengthof *ys + _Lengthof **ys;
}
