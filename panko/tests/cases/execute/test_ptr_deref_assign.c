// [[return: 27]]

int main() {
    long x = 42;
    long* p = &x;
    long** pp = &p;
    **pp = 27;
    return x;
}
