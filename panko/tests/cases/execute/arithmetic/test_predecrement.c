// [[arg: %d\n]]
// [[return: 40]]

int putchar(int);
int printf(char const*, ...);

int* f(int* p) {
    putchar(65);
    putchar(10);
    return p;
}

int main(int, char** argv) {
    int x = 42;
    // [[print: 41]]
    printf(argv[1], --x);
    // [[print: 41]]
    printf(argv[1], x);
    // [[print: A]]
    // [[print: 40]]
    printf(argv[1], --*f(&x));
    return x;
}
