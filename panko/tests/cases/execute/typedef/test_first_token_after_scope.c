int puts(char const*);

int main() {
    // [[print: hello]]
    {
        typedef int puts;
    }
    puts("hello");
}
