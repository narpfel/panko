// [[known-bug: compound literals don’t reject invalid storage class specifiers (this is UB)]]

int main() {
    // [[compile-error: invalid storage class specifier `extern` for compound literal]]
    (extern int){};
}
