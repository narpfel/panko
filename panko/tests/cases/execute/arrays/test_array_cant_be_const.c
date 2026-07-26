// [[known-bug: arrays that are const are broken (arrays should propagate their qualifiers to their element type]]

int main() {
    typeof(int[5]) const xs = {1, 2, 3, 4, 5};
    // [[compile-error: implicit conversion from `ptr<int const>` to `ptr<int>` drops qualifiers]]
    int* p = xs;
}
