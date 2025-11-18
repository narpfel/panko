// [[return: 1]]

int main() {
    struct T* p = nullptr;
    typeof(*p)* q = p;
    typeof_unqual(*p)* r = p;
    return p == q && p == r;
}
