// [[arg: %d\n]]
// [[arg: %lu\n]]

int printf(char const*, ...);

int main(int, char** argv) {
    char unsigned c = 42;
    int i = +c;
    // [[print: 42]]
    printf(*(argv + 1), i);
    unsigned long l = 42;
    unsigned long l2 = +l;
    // [[print: 42]]
    printf(*(argv + 2), l2);
}
