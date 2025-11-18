// [[return: 1]]

int main() {
    struct T* p = nullptr;
    struct T* q = &*p;
    return p == q;
}
