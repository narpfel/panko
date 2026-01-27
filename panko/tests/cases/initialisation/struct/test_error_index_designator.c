struct T {
    int x;
    int y;
};

int main() {
    struct T t = { [1] = 42 };
}
