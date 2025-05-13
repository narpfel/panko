// Test that the error message refers to function pointers as values, not
// functions.

int (*a)();
typedef int a;

typedef int b;
int (*b)();

int main() {
    int (*c)();
    typedef int c;

    typedef int d;
    int (*d)();
}
