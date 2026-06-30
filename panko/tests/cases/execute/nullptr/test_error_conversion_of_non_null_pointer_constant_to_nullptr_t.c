// [[known-bug]]

int main() {
    // [[compile-error: invalid implicit conversion from `ptr<void>` to `nullptr_t`]]
    typeof(nullptr) a = (void*)(int*)0;
}
