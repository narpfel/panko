int printf(char const*, ...);

#define MACRO(x, y, ...) __VA_OPT__() x + y

int main() {
    // [[print: 3]]
    printf("%d\n", MACRO(1, 2));
    // [[print: 7]]
    printf("%d\n", MACRO(2, 5, x));
    // [[print: 5]]
    printf("%d\n", MACRO(3, 2, 42, 123));
}
