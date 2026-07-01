int printf(char const*, ...);

int main() {
    int x = 42;
    int* p = &x;
    // [[print: 42]]
    printf("%d\n", *(1 ? p : 0));
    // [[print: 42]]
    printf("%d\n", *(0 ? (void*)0l : p));
}
