// [[return: 27]]

int main() {
    int x = 42;
    int* p = &x;
    int* q = &*p;
    *q = 27;
    return x;
}
