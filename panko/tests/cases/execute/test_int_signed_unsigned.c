int printf(char const*, ...);

int main() {
    int signed x = -42;
    int unsigned y = 27;

    // [[print: -42]]
    printf("%d\n", x);

    // [[print: 27]]
    printf("%u\n", y);
}
