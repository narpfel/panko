void f() {}

int main() {
    int x = 42;
    void* p = &x;
    *p = f();
}
