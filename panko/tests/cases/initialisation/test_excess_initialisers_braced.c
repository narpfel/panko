int main() {
    int x = {1, {2, 3}};

    int xs[2] = {1, 2, {3}, {4}};
    int ys[2][2] = {1, 2, 3, 4, {5}};
    int zs[3][2] = {{1, 2, {3, 6}}, {4, 5}};
    int qs[2] = {1, 2, {3, 4}, 5};
    int ws[3][2] = {{1, 2, {3, 6}, 70, 80}, {4, 5}, 6, 7, 8, 9};

    int excess_empty_braces_in_scalar = {42, {}};
    int excess_empty_braces_in_array[2] = {1, 2, {}};
    int excess_empty_braces_in_array_2[2] = {1, 2, {}, 4};

    int nested_braces_dont_emit_nested_errors = {1, {2, {3}}};
}
