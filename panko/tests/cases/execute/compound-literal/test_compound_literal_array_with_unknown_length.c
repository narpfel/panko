// [[known-bug]]

int printf(char const*, ...);

int main() {
    // [[print: 11]]
    printf("%zu\n", _Lengthof (int[]){[10] = 42});
    // [[print: 12]]
    printf("%zu\n", sizeof (char[]){"hello world"});
}
