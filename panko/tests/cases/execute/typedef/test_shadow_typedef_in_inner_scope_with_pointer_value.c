// [[return: 42]]

int printf(char const*, ...);

int main() {
    {
        typedef int name;
        {
            long place;
            long* name = &place;
            *name = -42;
            // [[print: -42]]
            printf("%ld\n", place);
        }
        name x = 123;
        // [[print: 123]]
        printf("%d\n", x);
    }
    int name = 42;
    return name;
}
