// [[arg: %c\n]]

int printf(char const*, ...);

int main(int, char** argv) {
    // [[print: %]]
    printf(argv[1], argv[1][0]);
    // [[print: c]]
    printf(argv[1], argv[1][1]);
    // [[print: \n]]
    printf(argv[1], argv[1][2]);
    // [[print: \0]]
    printf(argv[1], argv[1][3]);

    // s
    argv[1][1] = 115;
    // [[print: %s\n]]
    printf(argv[1], argv[1]);
}
