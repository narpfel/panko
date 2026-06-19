// [[nosnapshot]]
// [[known-bug]]

int main() {
    static int a;
    // [[compile-error: division by zero in constant expression]]
    static typeof(&a - &a) b = (int*)(1 / 0) - &a;
    // [[compile-error: shift count exceeds size of shifted type in constant expression]]
    static int c = (int*)(1 << 100) - &a;
}
