// [[known-bug: struct decl contained in `typeof` without declarators is ignored]]

typeof(struct T { int x; });

int main() {
    struct T x = {.x = 42};
    // [[return: 42]]
    return x.x;
}
