// [[return: 42]]

int printf(char const*, ...);

int main() {
    // [[print: 0]]
    printf("%d\n", false);
    // [[print: 1]]
    printf("%d\n", true);

    // [[print: 0]]
    printf("%d\n", _Generic(true + 1, int: 0, unsigned int: 1));
    // [[print: 1]]
    printf("%d\n", _Generic(true + 1u, int: 0, unsigned int: 1));

    return _Generic(true + false, int: 42, unsigned int: 27);
}
