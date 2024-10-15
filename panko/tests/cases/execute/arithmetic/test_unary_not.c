// [[arg: %d\n]]

int printf(char const*, ...);

int main(int, char** argv) {
    int i = 42;
    // [[print: 0]]
    printf(*(argv + 1), !i);
    // [[print: 1]]
    printf(*(argv + 1), !!i);
    // [[print: 0]]
    printf(*(argv + 1), !!!i);

    int j = !i;
    int k = !!i;
    int l = !!!i;
    // [[print: 0]]
    printf(*(argv + 1), j);
    // [[print: 1]]
    printf(*(argv + 1), k);
    // [[print: 0]]
    printf(*(argv + 1), l);
    // [[print: 1]]
    printf(*(argv + 1), !0);

    int* p = &i;
    // [[print: 0]]
    printf(*(argv + 1), !p);
    // [[print: 1]]
    printf(*(argv + 1), !!p);
    // [[print: 0]]
    printf(*(argv + 1), !!!p);

    char c = 42;
    // [[print: 0]]
    printf(*(argv + 1), !c);
    // [[print: 1]]
    printf(*(argv + 1), !!c);

    char c2 = 0;
    // [[print: 1]]
    printf(*(argv + 1), !c2);
    // [[print: 0]]
    printf(*(argv + 1), !!c2);

    char c3 = !27;
    // [[print: 0]]
    printf(*(argv + 1), c3);
}
