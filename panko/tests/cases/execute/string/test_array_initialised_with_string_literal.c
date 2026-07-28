// [[return: 103]]

int printf(char const*, ...);
int puts(char const*);

int main() {
    {
        char string[] = "string";
        // [[print: string]]
        printf("%s\n", string);
        // [[print: 7]]
        printf("%zu\n", _Lengthof string);
    }
    {
        char string[100] = "string";
        // [[print: string]]
        printf("%s\n", string);
        // [[print: 100]]
        printf("%zu\n", _Lengthof string);
    }

    {
        char string[] = {"string"};
        // [[print: string]]
        printf("%s\n", string);
        // [[print: 7]]
        printf("%zu\n", _Lengthof string);
    }

    char strings[][100] = {
        "first",
        "second",
        "third",
    };

    // [[print: first]]
    puts(strings[0]);
    // [[print: second]]
    puts(strings[1]);
    // [[print: third]]
    puts(strings[2]);

    {
        char strings[][5][100] = {
            [0] = "first",
            [1] = {[3] = "second"},
            "third",
        };

        // [[print: first]]
        puts(strings[0][0]);
        // [[print: second]]
        puts(strings[1][3]);
        // [[print: third]]
        puts(strings[2][0]);
    }

    return _Lengthof strings + _Lengthof strings[0];
}
