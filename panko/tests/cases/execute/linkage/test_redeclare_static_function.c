// [[return: 42]]

static int f();
static int f();

int main() {
    return f();
}

static int f() {
    return 42;
}
