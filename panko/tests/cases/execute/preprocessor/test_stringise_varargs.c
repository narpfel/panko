int puts(char const*);

#define M(x, ...) #__VA_ARGS__

int main() {
    // [[print: ]]
    puts(M(,));
    // [[print: ]]
    puts(M(1,));
    // [[print: hello]]
    puts(M(1, hello));
    // [[print: 2, 3]]
    puts(M(1, 2, 3));
    // [[print: hello, world]]
    puts(M(hello, hello, world));
}
