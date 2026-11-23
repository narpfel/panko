// struct members have no linkage, so as per 6.7.1p4, there shall be no more
// than one declaration of that member

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
