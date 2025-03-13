// [[return: 7]]

int puts(char const*);

int main() {
    "ignored string";

    // [[print: some string concatenated]]
    puts("some string" " " "concatenated");

    char* char_star = "char star";
    // [[print: char star]]
    puts(char_star);

    // [[print: char const star]]
    char const* char_const_star = "char const star";
    puts(char_const_star);
    return _Lengthof "string";
}
