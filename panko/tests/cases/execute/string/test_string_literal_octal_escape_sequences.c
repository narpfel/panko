// [[return: 2]]

int printf(char const*, ...);
int strcmp(char const*, char const*);

int main() {
    // [[print: <9]]
    printf("%s\n", "\749");
    // [[print: abcSghi]]
    printf("%s\n", "abc\123ghi");
    // [[print: 0]]
    printf("%d\12", strcmp("\12", "\n"));
    // [[print: 0]]
    printf("%d\12", strcmp("q\12q", "q\nq"));

    // check that concatenated strings don’t merge escape sequences
    char const* s = "\1" "1";
    // [[print: 11]]
    printf("%d%s\n", s[0], &s[1]);
    return _Lengthof "\12";
}
