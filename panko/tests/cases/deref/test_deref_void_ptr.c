int main() {
    int x = 42;
    void* p = &x;
    // TODO: GCC and clang allow this with a warning, but this is a constraint
    // violation because the result of unary * is an lvalue, but lvalues have
    // object type other than `void`.
    // chibicc rejects this.
    *p;
}
