// [[return: 42]]

#define PASTE(x, y) x ## y

int main() {
    // TODO: broken indentation
    "indented";
    PASTE(re, turn) 42;
    "also indented";
}
