// [[return: 42]]

int printf(char const*, ...);

int main() {
    {
        typedef char const name;
        name x = 'a';
        // [[print: a]]
        printf("%c\n", x);
    }

    int name = 42;
    return name;
}
