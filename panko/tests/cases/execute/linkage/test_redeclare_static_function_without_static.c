// [[return: 49]]

static int f();
int f();

static int g();
extern int g();

int main() {
    return f() + g();
}

int f() {
    return 42;
}

int g() {
    return 7;
}
