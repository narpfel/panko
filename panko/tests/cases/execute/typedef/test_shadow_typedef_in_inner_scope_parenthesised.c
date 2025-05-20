int main() {
    typedef int name;
    {
        int (name());
        _Generic(name(), int: 0);
    }
    {
        int (name)();
        _Generic(name(), int: 0);
    }
    {
        int (name[42]);
        _Lengthof(name);
    }
    {
        int (name)[42];
        _Lengthof(name);
    }
}
