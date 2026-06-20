// [[nosnapshot]]

int main() {
    // [[compile-error: division by zero in constant expression]]
    // [[compile-error: signed overflow in constant expression]]
    static int x = (static int){1 / 0} + (static int){0x7fff'ffff + 1};
}
