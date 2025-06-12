int printf(char const*, ...);

#define MACRO(x, y, ...) x __VA_OPT__(- y)

int main() {
    // [[print: 10]]
    printf("%d\n", MACRO(10, 10));
    // [[print: 4]]
    printf("%d\n", MACRO(10, 6, syntax error));
}
