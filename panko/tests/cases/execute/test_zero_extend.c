// [[arg: %lu\n]]

int printf(char const*, ...);

void print(char** argv, unsigned long l) {
    printf(*(argv + 1), l);
}

int main(int, char** argv) {
    char unsigned c = 42;
    // [[print: 42]]
    print(argv, c);
    unsigned short s = c;
    // [[print: 42]]
    print(argv, s);
    unsigned int i = c;
    // [[print: 42]]
    print(argv, i);
    unsigned int j = s;
    // [[print: 42]]
    print(argv, j);
    unsigned long l = c;
    // [[print: 42]]
    print(argv, l);
    unsigned long m = s;
    // [[print: 42]]
    print(argv, m);
    unsigned long n = i;
    // [[print: 42]]
    print(argv, n);
}
