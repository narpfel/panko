// [[known-bug]]

int main() {
    // [[compile-error: invalid storage class specifier `extern` for compound literal]]
    (extern int){};
}
