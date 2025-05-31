// [[return: 42]]

int putchar(int c);

#define PUTCHAR putchar
#define MACRO int x = 42;
#define ANOTHER_MACRO x + y

int main() {
    MACRO
    // [[print: a]]
    PUTCHAR('a');
    PUTCHAR('\n');
    return x;
}
