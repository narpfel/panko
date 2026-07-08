// [[known-bug: constexpr eval incorrectly accepts too many things as null pointer constants]]

int main() {
    // [[compile-error: invalid implicit conversion from `ptr<void>` to `nullptr_t`]]
    typeof(nullptr) a = (void*)(int*)0;
}
