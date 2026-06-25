int puts(char const*);

int main() {
    static char const* s = (static char const*){"hello world"};
    // [[print: hello world]]
    puts(s);
}
