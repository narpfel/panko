// [[return: 2]]

int printf(char const*, ...);

int main() {
    // [[print: abc?"'\'def]]
    printf("%s\n", "abc\?\"'\\\'def");
    return _Lengthof "\n";
}
