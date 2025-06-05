int printf(char const*, ...);

#define MACRO x

int MACRO = 42;

int main() {
    {
        typedef int MACRO;
        MACRO y = 27;
        // [[print: 27]]
        printf("%d\n", y);
    }
    // [[print: 42]]
    printf("%d\n", MACRO);
}
