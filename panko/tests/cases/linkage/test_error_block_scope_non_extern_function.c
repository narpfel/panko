int main() {
    void allowed();
    extern void also_allowed();

    static void f();
    // this does not emit the error again
    f();
}
