// [[known-bug]]

int main() {
    struct X* f(struct X { int x; }* p);
    // [[compile-error: invalid application of `sizeof` to incomplete type `struct X~2`]]
    sizeof *f(nullptr);
}
