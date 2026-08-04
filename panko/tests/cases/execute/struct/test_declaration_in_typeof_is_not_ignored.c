// [[return: 42]]

typeof(struct T { int x; });

int main() {
    struct T x = {.x = 42};
    return x.x;
}
