// [[return: 27]]

int main() {
    return _Generic(main, int(*)(): 27);
}
