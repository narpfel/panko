int printf(char const*, ...);

int main() {
    int array[][3] = {{1}, 2, 3};
    // [[print: 1]]
    printf("%d\n", array[0][0]);
    // [[print: 0]]
    printf("%d\n", array[0][1]);
    // [[print: 0]]
    printf("%d\n", array[0][2]);
    // [[print: 2]]
    printf("%d\n", array[1][0]);
    // [[print: 3]]
    printf("%d\n", array[1][1]);
    // [[print: 0]]
    printf("%d\n", array[1][2]);

    // [[print: 2 3]]
    printf("%zu %zu\n", _Lengthof array, _Lengthof array[0]);
}
