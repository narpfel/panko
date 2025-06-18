int printf(char const*, ...);

#define MACRO(x) #x

int main() {
    // [[print: x"""a\"b\\nc"]]
    printf("%s\n", MACRO(x"""a\"b\nc"));
    // [[print: '"']]
    printf("%s\n", MACRO('"'));
    // [[print: "\""]]
    printf("%s\n", MACRO("\""));
}
