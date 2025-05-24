typedef unsigned typedef_name;

int main() {
    int void a;
    void int b;
    short void c;
    short char d;
    char long e;
    int char f;
    char int g;
    int int h;
    unsigned short long i;
    unsigned signed int j;
    char short k;
    // TODO: this error is slightly silly and could be better
    signed unsigned l;
    typedef_name int m;
    short typedef_name n;
    unsigned long int long long x = 42;
}
