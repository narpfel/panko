int puts(char const*);

int main() {
    // [[print: hello world]]
    puts("hello\
 world");

    // [[print: hello world]]
    puts("hello\
\
 world");

    // [[print: hello world]]
    puts("\
hello world");

    // [[print: hello world]]
    puts("\
\
hello world");

    // [[print: hello world]]
    puts("hello world\
");

    // [[print: hello world]]
    puts("hello world\
\
");
}
