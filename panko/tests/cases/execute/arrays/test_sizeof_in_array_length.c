int printf(char const*, ...);

int main() {
    char cs[sizeof 0];
    int xs[sizeof(int[2])];
    // [[print: 4 8]]
    printf("%zu %zu\n", _Lengthof cs, _Lengthof xs);
}
