void f() {}

int main() {
    42 || f();
    f() && f();
    f() && 27;
}
