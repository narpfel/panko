// [[arg: %zu\n]]
// [[return: 8]]

int printf(char const*, ...);
void* calloc(unsigned long, unsigned long);

int main(int, char** argv) {
    // [[print: 1]]
    printf(*(argv + 1), sizeof(char));
    // [[print: 1]]
    printf(*(argv + 1), sizeof(char const));
    // [[print: 8]]
    printf(*(argv + 1), sizeof(char const*));
    // [[print: 8]]
    printf(*(argv + 1), sizeof(char (*)));
    // [[print: 8]]
    printf(*(argv + 1), sizeof(void (*)(int, int)));
    // [[print: 2]]
    printf(*(argv + 1), sizeof(unsigned short));
    // [[print: 4]]
    printf(*(argv + 1), sizeof(int));
    // [[print: 8]]
    printf(*(argv + 1), sizeof(int long));
    // [[print: 8]]
    printf(*(argv + 1), sizeof(long long));
    // [[print: 8]]
    printf(*(argv + 1), sizeof(void*));

    // [[print: 4]]
    printf(*(argv + 1), sizeof 42);
    // [[print: 4]]
    printf(*(argv + 1), sizeof 42u);
    // [[print: 8]]
    printf(*(argv + 1), sizeof 42l);
    // [[print: 8]]
    printf(*(argv + 1), sizeof sizeof 42);

    int* p = calloc(1, sizeof *p);
    return *p + sizeof &calloc;
}
