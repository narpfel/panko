int printf(char const*, ...);

int global_name = 10;

int main() {
    static int* ptr = &global_name;
    // [[print: 10]]
    printf("%d\n", *ptr);
    ++*ptr;
    // [[print: 11]]
    printf("%d\n", global_name);
}
