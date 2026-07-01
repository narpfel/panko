int printf(char const*, ...);

int main() {
    int x = 42;
    int* p = &x;
    // [[print: 42]]
    printf("%d\n", *(1 ? p : 0));
    // [[print: 42]]
    printf("%d\n", *(0 ? (void*)0l : p));

    // test that pointers to statics are not null pointer constants
    static int y;
    // [[print: 0]]
    printf("%d\n", _Generic(0 ? (void*)&y : &y, void*: 0));
}
