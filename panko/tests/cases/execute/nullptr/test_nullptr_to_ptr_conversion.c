int printf(char const*, ...);
int puts(char const*);

int main() {
    int* p = nullptr;
    // [[print: (nil)]]
    printf("%p\n", (void*)p);
    // [[print: falsy]]
    puts(p ? "truthy" : "falsy");
    // [[print: truthy]]
    puts(&p ? "truthy" : "falsy");
    // [[print: nullptr]]
    puts(nullptr ? "someptr" : "nullptr");
    return (unsigned long)(int*)nullptr;
}
