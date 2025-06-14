// [[return: 6]]

int printf(char const*, ...);

#define MACRO(x, y, ...) x + y __VA_OPT__(+) __VA_ARGS__

#define EMPTY_OBJECT
#define EMPTY_FUNCTION()

#define NESTED_EMPTY EMPTY_OBJECT EMPTY_FUNCTION()

int main() {
    // [[print: 3]]
    printf("%d\n", MACRO(1, 2));
    // [[print: 3]]
    printf("%d\n", MACRO(1, 2, ));
    // [[print: 3]]
    printf("%d\n", MACRO(1, 2, EMPTY_OBJECT));
    // [[print: 3]]
    printf("%d\n", MACRO(1, 2, EMPTY_FUNCTION()));
    // [[print: 3]]
    printf("%d\n", MACRO(1, 2, NESTED_EMPTY));
    return MACRO(1, 2, 3 + 4, 5, 6);
}
