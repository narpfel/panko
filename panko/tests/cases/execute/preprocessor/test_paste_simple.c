// [[return: 45]]

int printf(char const*, ...);

#define PASTE(x, y) x ## y

int main() {
    int variable = 42;
    // [[print: 42]]
    printf("%d\n", PASTE(,) variable);
    // [[print: -42]]
    printf("%d\n", PASTE(-,) variable);
    // [[print: -42]]
    printf("%d\n", PASTE(,-) variable);
    // [[print: 43]]
    printf("%d\n", PASTE(+,+) variable);

    return PASTE(var, iable) + 2;
}
