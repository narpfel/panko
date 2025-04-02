// [[arg: %zu\n]]

int printf(char const*, ...);

int main(int, char** argv) {
    // [[print: 1]]
    printf(*(argv + 1), alignof(char));
    // [[print: 1]]
    printf(*(argv + 1), alignof(char const));
    // [[print: 8]]
    printf(*(argv + 1), alignof(char const*));
    // [[print: 8]]
    printf(*(argv + 1), alignof(char (*)));
    // [[print: 8]]
    printf(*(argv + 1), alignof(void (*)(int, int)));
    // [[print: 2]]
    printf(*(argv + 1), alignof(unsigned short));
    // [[print: 4]]
    printf(*(argv + 1), alignof(int));
    // [[print: 8]]
    printf(*(argv + 1), alignof(int long));
    // [[print: 8]]
    printf(*(argv + 1), alignof(long long));
    // [[print: 8]]
    printf(*(argv + 1), alignof(void*));

    // TODO: this should be `size_t`
    _Generic(alignof(int), unsigned long: 0);
}
