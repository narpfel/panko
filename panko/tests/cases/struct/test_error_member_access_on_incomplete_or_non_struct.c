int main() {
    struct Incomplete value;
    value.member;
    struct Incomplete* pointer;
    pointer->member;
    int x = 42;
    x.member;
    true.member;
}
