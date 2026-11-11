void function() {
    // [[compile-error: static assertion failed: all pointers have the same size]]
    static_assert(sizeof &function != sizeof(int*), "all pointers have the same size");
}
