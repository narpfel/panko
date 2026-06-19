// [[nosnapshot]]

int main() {
    static int a;
    // [[compile-error: division by zero in constant expression]]
    static int b = &a == (int*)(1 / 0);
    // [[compile-error: shift count exceeds size of shifted type in constant expression]]
    // [[compile-error: signed overflow in constant expression]]
    static int c = (int*)(1 << 100) != (int*)(0x7fff'ffff + 1);
}
