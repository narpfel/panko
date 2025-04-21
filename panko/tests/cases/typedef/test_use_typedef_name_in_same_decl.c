// TODO: This is a bit of a weird case. We test that `i32` is a `typedef-name`
// in `array`’s declarator by checking the typechecker’s sexpr representation.
// However, because constexpr evaluation is not implemented yet, any use of the
// type `array` will ICE.
typedef int i32, array[sizeof(i32)];

// int main() {
//     return _Lengthof(array);
// }
