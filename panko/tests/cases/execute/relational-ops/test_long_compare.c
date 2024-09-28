// [[return: 4]]

int main() {
    int i1 = 42;
    int i2 = 27;
    long p1 = 5;
    long p2 = 6;
    return (p1 != i1) + (p2 != i2) + (p1 + 1 == p2) + (p2 - 1 == p1);
}
