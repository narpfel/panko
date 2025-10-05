static int x;
int x;

int y();
static int y();

int f(int x) {
    static int x;
    return x;
}

int main() {
    extern int z;
    static int z;

    static int w;
    int w;

    // use once to test that using the name does not re-emit the diagnostic
    x; y; z; w;
}
