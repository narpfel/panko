// [[known-bug]]
// [[return: 1]]

int printf(char const*, ...);

int main() {
    typeof(typeof(char const*)[1]) s[] = {"hello world"};
    // [[print: hello world])
    printf("%s\n", s[0][0]);
    return _Lengthof(s);
}
