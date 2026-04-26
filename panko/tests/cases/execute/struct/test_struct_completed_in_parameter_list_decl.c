int main() {
    struct X* f(struct X { int x; }* p);
    // [[compile-error: invalid application of `sizeof` to incomplete type `struct X~2`]]
    sizeof *f(nullptr);

    // [[compile-error: cannot declare variable `incomplete_here` with incomplete type `struct X~2`]]
    struct X incomplete_here = {};
}
