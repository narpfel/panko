void f() {
    struct T;
    // [[compile-error: redeclaration of `struct T~\d+` with different tag `union`]]
    union T;
}

void g() {
    struct U {
        int x;
        int y;
    };
    // [[compile-error: redeclaration of `struct U~\d+ complete` with different tag `union`]]
    union U value;
}

void h() {
    union V {
        int x;
        unsigned y;
    };
    // [[compile-error: redeclaration of `union V~\d+ complete` with different tag `struct`]]
    struct V {
        int x;
        unsigned y;
    };
}

void i() {
    union Name;
    // [[compile-error: redeclaration of `union Name~\d+` with different tag `struct`]]
    struct Name {
        int member;
    };
}

int main() {}
