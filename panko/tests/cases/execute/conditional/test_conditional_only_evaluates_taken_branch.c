// [[arg: %d\n]]

int printf(char const*, ...);

int main(int, char** argv) {
    // [[print: 2]]
    0 ? printf(argv[1], 1) : printf(argv[1], 2);
    // [[print: 3]]
    42 ? printf(argv[1], 3) : printf(argv[1], 4);
}
