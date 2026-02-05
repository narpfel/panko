// [[known-bug]]
// [[compile-error: cannot assign to `const` value `this_is_const`]]

int main() {
    typeof_unqual(42) const this_is_const;
    this_is_const = 42;
    return this_is_const;
}
