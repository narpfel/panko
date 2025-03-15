// [[return: 2]]

int printf(char const*, ...);
int strcmp(char const*, char const*);

int main() {
    // [[print: a]]
    printf("%s\n", "\x61");
    // [[print: abcaghi]]
    printf("%s\n", "abc\x61ghi");
    // [[print: 0]]
    printf("%d\x00000000a", strcmp("\x00000000a", "\n"));
    // [[print: 0]]
    printf("%d\x00000000a", strcmp("q\xaq", "q\nq"));
    return _Lengthof "\x00000000a";
}
