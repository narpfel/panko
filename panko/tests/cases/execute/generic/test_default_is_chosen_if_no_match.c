// [[return: 42]]

int main() {
    char c = 0;
    return _Generic(c, int: 1, long: 2, default: 42);
}
