// [[return: 42]]

int main() {
    int xs[5] = {42};
    typeof(int[5])* p1 = &xs;
    // the added qualifier in the element type makes the pointee types unequal
    // but compatible, so the implicit conversion should be allowed
    typeof(int const[5])* p2 = p1;
    return **p2;
}
