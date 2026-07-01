// [[return: 1]]

int main() {
    int* a = 0;
    bool* b = false;
    char const* c = (void*)0;
    long* const d = nullptr;
    typeof(int())* e = 42 ? 0 : 1;
    return !a && !b && !c && !d && !e;
}
