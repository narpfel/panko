// [[return: 42]]

int printf(char const*, ...);

int main() {
    {
        typedef int name;
        {
            long name;
            name = -42;
            // [[print: -42]]
            printf("%ld\n", name);
        }
        name x = 123;
        // [[print: 123]]
        printf("%d\n", x);
    }
    int name = 42;
    return name;
}
