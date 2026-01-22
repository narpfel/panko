struct T {
    int x;
};

struct U {
    int x;
    char c;
};

int main() {
    (int())42;

    struct T t;
    struct T t2;

    (struct T)t;
    (struct T const)t;
    (typeof(t2))t;
    (struct U)t;
    (int)t;
}
