int printf(char const*, ...);

int main() {
    int xs[][2] = {
        1, 2,
        {3},
        5, 6,
    };

    // [[print: 3 2]]
    printf("%zu %zu\n", _Lengthof xs, _Lengthof xs[0]);
    // [[print: 1 2]]
    printf("%d %d\n", xs[0][0], xs[0][1]);
    // [[print: 3 0]]
    printf("%d %d\n", xs[1][0], xs[1][1]);
    // [[print: 5 6]]
    printf("%d %d\n", xs[2][0], xs[2][1]);
}
