// [[arg: %d\n]]
// [[return: 44]]

int putchar(int);
int printf(char const*, ...);

int* f(int* p) {
    putchar(65);
    putchar(10);
    return p;
}

int main(int, char** argv) {
    int x = 42;
    // [[print: 43]]
    printf(argv[1], ++x);
    // [[print: 43]]
    printf(argv[1], x);
    // [[print: A]]
    // [[print: 44]]
    printf(argv[1], ++*f(&x));
    return x;
}
