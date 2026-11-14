struct T {
    int x;
    // [[compile-error: static assertion failed: in struct member list]]
    static_assert(42 == 27, "in struct member list");
    int y;
};

union U {
    int x;
    // [[compile-error: static assertion failed: in union member list]]
    static_assert(42 == 27, "in union member list");
    char cs[sizeof(int)];
};
