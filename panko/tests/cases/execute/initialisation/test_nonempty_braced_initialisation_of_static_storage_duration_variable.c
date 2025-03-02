int printf(char const*, ...);

int xs[100] = {42, 27, 5, 2147483647};

int value = {123};

int main(int, char**) {
    char s[100] = {37, 100, 10};
    // [[print: 42]]
    printf(s, xs[0]);
    // [[print: 27]]
    printf(s, xs[1]);
    // [[print: 5]]
    printf(s, xs[2]);
    // [[print: 2147483647]]
    printf(s, xs[3]);
    // [[print: 0]]
    printf(s, xs[4]);
    // [[print: 0]]
    printf(s, xs[99]);
    // [[print: 123]]
    printf(s, value);
}
