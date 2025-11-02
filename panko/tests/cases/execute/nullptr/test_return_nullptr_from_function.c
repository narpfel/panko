// [[return: 1]]

typeof(nullptr) f() {
    return nullptr;
}

int main() {
    return f() == nullptr;
}
