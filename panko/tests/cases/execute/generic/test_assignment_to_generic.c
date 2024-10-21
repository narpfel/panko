// [[return: 42]]

int main() {
    int value = 27;
    _Generic(123, int: value, short: 123) = 42;
    return value;
}
