int main() {
    int x = {1, {2, 3}};
    // TODO: nested braced initialisers lead to incorrect indices for following initialisers
    int xs[2] = {1, 2, {3}, {4}};
    int ys[2][2] = {1, 2, 3, 4, {5}};
    int zs[3][2] = {{1, 2, {3, 6}}, {4, 5}};
    int qs[2] = {1, 2, {3, 4}, 5};
    int ws[3][2] = {{1, 2, {3, 6}, 70, 80}, {4, 5}, 6, 7, 8, 9};
}
