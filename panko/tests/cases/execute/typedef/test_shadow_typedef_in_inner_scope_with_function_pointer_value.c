// [[return: 42]]

typedef int name;

int f(int name) {
    return name * 2;
}

int main() {
    int (*name)(int) = f;
    return name(21);
}
