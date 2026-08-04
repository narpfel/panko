// [[known-bug: the first declarator in an init-declarator-list is erroneously ignored if it is abstract]]

int main() {
    // [[compile-error: declaration does not specify a name]]
    // [[compile-error: this looks like a declaration with type `int`]]
    int;

    // [[compile-error: declaration does not specify a name]]
    // [[compile-error: this looks like a declaration with type `long`]]
    long, *p;
}
