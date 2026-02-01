int printf(char const*, ...);

int main() {
    struct T {
        typeof(char const*) s[1];
    } t = {"hello world"};
    // [[print: hello world]]
    printf("%s\n", *(char const**)&t);
}
