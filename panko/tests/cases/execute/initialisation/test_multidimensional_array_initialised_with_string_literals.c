int printf(char const*, ...);
int puts(char const*);

int main() {
    {
        char strings[][2][100] = {
            [0] = "first",
            "also first",
            {"second"},
            "third",
            "also third",
        };

        // [[print: first]]
        puts(strings[0][0]);
        // [[print: also first]]
        puts(strings[0][1]);
        // [[print: second]]
        puts(strings[1][0]);
        // [[print: ]]
        puts(strings[1][1]);
        // [[print: third]]
        puts(strings[2][0]);
        // [[print: also third]]
        puts(strings[2][1]);

        // [[print: 3 2 100]]
        printf("%zu %zu %zu\n", _Lengthof strings, _Lengthof strings[0], _Lengthof strings[0][0]);
    }

    {
        char strings[][100] = {"string", {"another string"}};
        // [[print: 2]]
        printf("%zu\n", _Lengthof strings);
        // [[print: string]]
        puts(strings[0]);
        // [[print: another string]]
        puts(strings[1]);
    }
}
