struct T {
    int x;
    int y;
    // [[compile-error: duplicate member declaration for `x`]]
    // [[compile-error: previously declared here]]
    char x;
};

struct U {
    int redeclared_in_anonymous;
    // [[compile-error: duplicate member declaration for `redeclared_in_anonymous`]]
    struct { long redeclared_in_anonymous; };
};
