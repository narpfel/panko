// [[return: 6]]

int printf(char const*, ...);

#define MACRO(x, y, ...) x + y __VA_OPT__(+) __VA_ARGS__

int main() {
    // [[print: 3]]
    printf("%d\n", MACRO(1, 2));
    return MACRO(1, 2, 3 + 4, 5, 6);
}
