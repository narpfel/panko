// [[return: 27]]

int main() {
    int x = 42;
    int* p = &x;
    *&*p = 27;
    return x;
}
