int printf(char const*, ...);

typedef int i32, array_with_length[42], array_with_unknown_length[];
typedef i32* pointer;

void check_pointer_decay_of_typedefd_arrays(array_with_length xs) {
    printf("%zu\n", sizeof xs);
    _Generic(xs, pointer: 0);
    _Generic(xs, i32*: 0);
    _Generic(xs, int*: 0);
}

void check_pointer_decay_of_typedefd_arrays_with_unknown_length(array_with_unknown_length xs) {
    printf("%zu\n", sizeof xs);
    _Generic(xs, pointer: 0);
    _Generic(xs, i32*: 0);
    _Generic(xs, int*: 0);
}

int main(int, char**) {
    array_with_unknown_length a = {1, 2, 3};
    array_with_unknown_length b = {1, 2, 3, 4};
    // [[print: 8]]
    check_pointer_decay_of_typedefd_arrays(a);
    // [[print: 8]]
    check_pointer_decay_of_typedefd_arrays(b);
    // [[print: 8]]
    check_pointer_decay_of_typedefd_arrays_with_unknown_length(a);
    // [[print: 8]]
    check_pointer_decay_of_typedefd_arrays_with_unknown_length(b);
    // [[print: 3 4]]
    printf("%zu %zu\n", _Lengthof a, _Lengthof b);
}
