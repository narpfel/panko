// [[return: 42]]

int printf(char const*, ...);

int main() {
    {
        typedef int name;
        {
            typedef char const name;
            name x = 'a';
            // [[print: a]]
            printf("%c\n", x);
        }
        name x = 123;
        // [[print: 123]]
        printf("%d\n", x);
    }
    int name = 42;
    return name;
}
