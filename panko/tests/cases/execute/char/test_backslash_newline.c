int printf(char const*, ...);

int main() {
    // [[print: a]]
    printf("%c\n", '\
a');

    // [[print: b]]
    printf("%c\n", 'b\
');
}
