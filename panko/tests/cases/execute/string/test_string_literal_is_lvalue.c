int puts(char const*);

int main() {
    // [[print: hello]]
    puts(&"hello");
}
