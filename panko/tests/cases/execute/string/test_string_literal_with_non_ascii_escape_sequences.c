int strcmp(char const*, char const*);
int printf(char const*, ...);
int puts(char const*);

int main() {
    // [[print: 2]]
    printf("%zu\n", sizeof "\xe4");
    // [[print: -28]]
    printf("%d\n", "\xe4"[0]);
    // [[print: 0]]
    printf("%d\n", "\xe4"[1]);

    // [[print: ä]]
    puts("ä");
    // [[print: ä]]
    puts("\xc3\xa4");
    // [[print: 3]]
    printf("%zu\n", sizeof "ä");
    // [[print: -61]]
    printf("%d\n", "ä"[0]);
    // [[print: -92]]
    printf("%d\n", "ä"[1]);
    // [[print: 0]]
    printf("%d\n", "ä"[2]);

    // [[print: 0]]
    printf("%d\n", strcmp("ä", "\xc3\xa4"));
}
