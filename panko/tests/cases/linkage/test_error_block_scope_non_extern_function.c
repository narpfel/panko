int main() {
    void allowed();
    extern void also_allowed();

    static void f();
    // TODO: this should not emit the error again
    f();
}
