// [[known-bug]]

struct Struct {
    int member;
};

struct Struct make_struct();

int main() {
    // [[compile-error: Error: cannot assign to this expression because it is not an lvalue]]
    make_struct().member = 42;
    struct Struct value = {};
    // [[compile-error: Error: cannot assign to this expression because it is not an lvalue]]
    (true ? value : value).member = 27;
}
