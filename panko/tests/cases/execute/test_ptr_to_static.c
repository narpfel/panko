// [[return: 96]]

int x = 42;

int main() {
    int* p = &x;
    int local_copy = *p;
    *p = 27;
    return local_copy + *p + x;
}
