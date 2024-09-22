// [[return: 1]]

int main() {
    long* p1;
    unsigned long long b = 42;
    long* p2 = b + p1;
    long* p3 = p2 - b;
    return p3 == p1;
}
