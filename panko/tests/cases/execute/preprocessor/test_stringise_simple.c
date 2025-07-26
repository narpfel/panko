// [[return: 6]]

int printf(char const*, ...);

#define MACRO(x) #x

int main() {
    // [[print: a; 42 + 27; b]]
    printf("%s\n", MACRO(a; 42 + 27; b));
    // [[print: something]]
    printf("%s\n", MACRO(some) MACRO(thing));
    return _Lengthof(MACRO(a b c));
}
