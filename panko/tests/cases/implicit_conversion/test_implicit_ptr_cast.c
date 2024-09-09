int const* const_can_be_added() {
    int* p;
    return p;
}

int* const_can_be_removed() {
    int const* p;
    return p;
}

char** cast_between_completely_different_ptr_types() {
    int* p;
    return p;
}
