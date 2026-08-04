// [[known-bug: structs defined in compound literals are completed only after the containing decl’s type is resolved]]
// [[return: 123]]

int main() {
    struct T* p = &(struct T { int x; }){123};
    // [[print: 123]]
    return p->x;
}
