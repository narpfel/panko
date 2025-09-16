int printf(char const*, ...);

extern int x;

int main() {
    // [[print: 42]]
    printf("%d\n", x);
}
