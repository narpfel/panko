// [[known-bug: checking for redeclarations not implemented yet]]
// [[compile-error: incompatible redefinition of `struct T~\d+`]]

struct T {
    int x;
};

struct T {
    long l;
};
