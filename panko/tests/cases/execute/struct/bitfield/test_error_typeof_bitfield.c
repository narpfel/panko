// [[known-bug]]

struct T {
    int x:20;
};

int main() {
    struct T value = {};
    // [[compile-error: `typeof` cannot be applied to bitfields]]
    typeof(value.x) x = value.x;
}
