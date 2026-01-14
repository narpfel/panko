// [[known-bug]]
// [[compile-error: cannot declare variable `v` with incomplete type `void`]]
// [[compile-error: cannot declare variable `v2` with incomplete type `void`]]
// [[nosnapshot]]

int main() {
    void v = {};
    void v2 = {[42] = 27};
}
