// [[known-bug]]
// [[compile-error: invalid integer suffix `\+name`]]

int main() {
    int name = 12;
    return 0x1e+name;
}
