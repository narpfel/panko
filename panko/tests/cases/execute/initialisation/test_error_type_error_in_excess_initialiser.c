// [[known-bug]]
// [[compile-error: argument count mismatch: too many arguments to function call (expected 0 but got 1)]]

void function();

int main() {
    int x = {1, function(42)};
}
