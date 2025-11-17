// [[return: 123]]

int printf(char const*, ...);

int main() {
    int xs[42] = {1, 2, 3, 4, [27] = 5};
    typeof(int[])* p = &xs;

    // [[print: 5]]
    printf("%d\n", (*p)[27]);
    // [[print: 1]]
    printf("%d\n", **p);
    **p = 123;
    return **p;
}
