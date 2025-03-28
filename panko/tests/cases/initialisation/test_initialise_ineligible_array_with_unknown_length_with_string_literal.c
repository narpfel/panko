// TODO: this test should produce the same diagnostics as
// `test_initialise_ineligible_array_with_string_literal.c`,
// but the order in which the typechecking is performed means
// that additional errors for creating zero-sized array are
// emitted and the correct errors are duplicated due to the
// duplicate type checking performed on arrays with unknown
// length.

int main() {
    short abc[] = "abc";
    int xs[] = "abc";
    int ys[] = {"abc"};
    int zs[][100] = {"abc", {"d"}};
    int ws[][100] = {
        {"abc"},
        {"d"},
    };
    int multidimensional[][4] = "abc";
    int another_multidimensional[][5] = {"def"};
}
