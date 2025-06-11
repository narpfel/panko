int printf(char const*, ...);

#define MACRO(x, y, ...) __VA_OPT__((3) -) x + y

int main() {
    // [[print: 3]]
    printf("%d\n", MACRO(1, 2));
    // [[print: 6]]
    printf("%d\n", MACRO(2, 5, x));
    // [[print: 4]]
    printf("%d\n", MACRO(1, 2, 42, 123));
}
