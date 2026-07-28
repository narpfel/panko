int const* const_can_be_added() {
    int* p;
    return p;
}

void* implicit_cast_to_void_pointer() {
    int* p;
    return p;
}

int* implicit_cast_from_void_pointer(void* p) {
    return p;
}
