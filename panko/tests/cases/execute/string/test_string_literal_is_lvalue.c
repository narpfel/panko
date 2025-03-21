int puts(char const*);

int main() {
    // [[print: hello]]
    puts(&"hello");
    // [[print: world]]
    puts(&*"world");
}
