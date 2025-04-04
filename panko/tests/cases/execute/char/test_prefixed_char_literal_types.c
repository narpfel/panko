int main() {
    _Generic(u8'a', char unsigned: 0);
    _Generic(u'a', unsigned short: 0);
    _Generic(U'a', unsigned int: 0);
    _Generic(L'a', int: 0);
}
