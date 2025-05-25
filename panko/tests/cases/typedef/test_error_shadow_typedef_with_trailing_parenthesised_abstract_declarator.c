typedef unsigned typedef_name;

int main() {
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
