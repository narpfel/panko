extern int puts(char const*);

int main() {
    extern int printf(char const*, ...);
    extern int x;

    // [[print: it works]]
    puts("it works");
    // [[print: 42]]
    printf("%d\n", x);
}

int x = 42;
