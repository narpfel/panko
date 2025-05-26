int puts(char const*);
int printf(char const*, ...);

int main() {
    // [[print: 61]]
    printf("%x\n", u\
8'a');
    // [[print: a]]
    puts("\x6\
1");

    // [[print: hello world]]
    pu\
t\
\
s\
("hell\
\
o\
\
 world");

    // [[print: hello world]]
    puts\
(\
"\
hello world");
}
