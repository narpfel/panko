int main() {
    typedef int name;
    {
        int ((name())());
    }
    {
        int ((name[42])());
    }
}
