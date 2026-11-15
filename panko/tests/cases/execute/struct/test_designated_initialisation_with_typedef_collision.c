// [[known-bug: designated initialisers should accept `AnyIdentifier` (so that `typedef`’d names work)]]

struct T { int x; };

typedef long x;

int main() {
    struct T t = { .x = 42 };
    // [[return: 42]]
    return t.x;
}
