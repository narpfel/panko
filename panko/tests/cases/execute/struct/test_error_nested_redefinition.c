// [[known-bug: the different definitions of `struct T` are not resolved to the same `Id`]]

int main() {
    // [[compile-error: nested redefinition of `struct T~\d+ complete`]]
    struct T xs[sizeof(struct T { int xs[sizeof(struct T { long l; })]; })];
    return _Lengthof xs;
}
