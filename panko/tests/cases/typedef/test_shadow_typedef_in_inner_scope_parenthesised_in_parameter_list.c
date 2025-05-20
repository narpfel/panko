int main() {
    typedef int name;
    {
        int f(name());
        int g(int (name));
        int h(name (name));
        int i(name name);
    }
}
