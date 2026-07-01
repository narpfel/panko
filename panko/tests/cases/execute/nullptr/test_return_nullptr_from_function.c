// [[return: 1]]

typeof(nullptr) f() {
    return nullptr;
}

int main() {
    int* null = f();
    return f() == nullptr && !null;
}
