// [[known-bug]]

// The pointer compatibility constraints for `simple-assignment` from §6.5.17.2
// are not implemented.

int main() {
    int const x = 42;
    // [[compile-error: invalid implicit conversion from `ptr<int const>` to `ptr<int>`]]
    int* p = &x;
}
