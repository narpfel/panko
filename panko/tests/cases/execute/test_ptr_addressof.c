// [[return: 4]]

int main() {
    int i1 = 42;
    int i2 = 27;
    int* p1 = &i1;
    int* p2 = &i2;
    return (p1 == &i1) + (p2 == &i2) + (p1 + 1 == p2) + (p2 - 1 == p1);
}
