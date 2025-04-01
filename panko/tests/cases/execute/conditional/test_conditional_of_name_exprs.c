int printf(char const*, ...);

int main() {
    {
        int x = 42;
        int y = 27;
        // [[print: 27]]
        printf("%d\n", 0 ? x : y);
        // [[print: 42]]
        printf("%d\n", 1 ? x : y);
    }
    {
        short x = 42;
        short y = 27;
        // [[print: 27]]
        printf("%d\n", 0 ? x : y);
        // [[print: 42]]
        printf("%d\n", 1 ? x : y);
    }
    {
        long x = 42;
        long y = 27;
        // [[print: 27]]
        printf("%ld\n", 0 ? x : y);
        // [[print: 42]]
        printf("%ld\n", 1 ? x : y);
    }
}
