int static_int = {1, 2};
int static_array[2] = {1, 2, 3};

int main() {
    int x = {1, 2};
    int xs[2] = {1, 2, 3, 4};
    int ys[2][2] = {1, 2, 3, 4, 5};
    int zs[3][2] = {{1, 2, 3}, {4, 5}};

    // TODO: this error message is wrong
    int scalar_in_array_initialiser[2] = {1, {2, 3}};
    int nested_braces_in_first_initialiser[2] = {{{1, 2}}};

    int multiline[3][2] = {
        {1, 2, 3},
        {4, 5},
    };
}
