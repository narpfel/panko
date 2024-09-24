// [[return: 42]]

int main() {
    long x = 42;
    long* p = &x;
    long** pp = &p;
    return **pp;
}
