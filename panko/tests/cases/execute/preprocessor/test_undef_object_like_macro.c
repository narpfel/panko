int printf(char const*, ...);

int main() {
    int x = 42;
    int y = 27;
#define x y
    // [[print: 27, 27]]
    printf("%d, %d\n", x, y);
#undef x
    // [[print: 42, 27]]
    printf("%d, %d\n", x, y);
}
