typedef unsigned typedef_name;

int main() {
    // TODO: these should all error
    {
        int typedef_name(());
    }
    {
        int typedef_name([42]);
    }
    {
        int typedef_name((int));
    }
}
