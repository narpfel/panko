int printf(char const*, ...);

#define MACRO(x, y, ...) x + y

int main() {
    // [[print: 5]]
    printf("%d\n", MACRO(2, 3));
    // [[print: 10]]
    printf("%d\n", MACRO(3, 7, 123));
    // [[print: 15]]
    printf("%d\n", MACRO(7, 8, syntax (nested, parens) error, 123));
}
