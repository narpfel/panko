// Test that the error message refers to function pointers as values, not
// functions.

int (*a)();
typedef int a;

// TODO: this case is not implemented yet
// typedef int b;
// int (*b)();

int main() {
    int (*c)();
    typedef int c;

    // TODO: this case is not implemented yet
    // typedef int d;
    // int (*d)();
}
