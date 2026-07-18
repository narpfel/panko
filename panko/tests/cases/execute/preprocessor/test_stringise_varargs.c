int puts(char const*);

#define M(x, ...) #__VA_ARGS__

#define STRINGISE_VA_OPT(x, ...) "\"" #__VA_OPT__(x) "\" => \"" #__VA_ARGS__ "\""

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

    // [[print: "" => ""]]
    puts(STRINGISE_VA_OPT(hello world));
    // [[print: "" => ""]]
    puts(STRINGISE_VA_OPT(hello world,));
    // [[print: "hello world" => ",,"]]
    puts(STRINGISE_VA_OPT(hello world,,,));
    // [[print: "hello world" => "not, empty"]]
    puts(STRINGISE_VA_OPT(hello world, not, empty));
}
