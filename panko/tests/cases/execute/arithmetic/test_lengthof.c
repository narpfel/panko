// [[return: 2]]

int main() {
    // TODO: this should be `size_t`
    _Generic(_Lengthof(int[42]), unsigned long: 0);
    int xs[] = {1, 2};
    _Generic(_Lengthof xs, unsigned long: 0);
    return _Lengthof xs;
}
