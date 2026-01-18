struct T* ptr;

void f() {
    struct T {
        int x, y;
    };
    // [[compile-error: Error: invalid application of `sizeof` to incomplete type `struct T~\d+`]]
    // [[compile-error: Error: dereference of pointer to incomplete type `struct T~\d+`]]
    sizeof *ptr;
}
