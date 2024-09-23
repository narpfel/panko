// [[return: 4]]

int main() {
    long* p1;
    unsigned long long b = 42;
    long* p2 = b + p1;
    long* p3 = p2 - b;
    return (p1 < p2) + (p1 <= p3) + (p2 > p1) + (p3 >= p1);
}
