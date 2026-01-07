struct T;

void f1() {
    struct T* p1;
    struct T* p2;
}

void f2() {
    struct T;
    struct T* p1;
    struct T* p2;
}

void forward_decl_in_local_scope() {
    struct U;
    {
        struct U* p1;
        struct U* p2;
    }
    {
        struct U* p1;
        struct U* p2;
    }
}
