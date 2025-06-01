// [[return: 27]]

int printf(char const*, ...);

#define MACRO int x

int main() {
    MACRO = 42;
    int* p = &x;

#define x y

    MACRO = 27;

    // [[print: *p: 42]]
    printf("*p: %d\n", *p);
    // [[print: x: 27]]
    printf("x: %d\n", x);
    // [[print: y: 27]]
    printf("y: %d\n", y);
    return x;
}
