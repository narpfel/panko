int puts(char const*);

void extern_fn();

int main() {
    // [[print: hello]]
    puts("hello");
    // [[print: world]]
    extern_fn();
}
