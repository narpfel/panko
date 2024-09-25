// [[return: 42]]

int main() {
    int x = 42;
    int* p = &x;
    int* q = &*p;
    return *q;
}
