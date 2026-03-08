// [[return: 42]]

struct Struct {
    int value;
};

struct Struct make_struct() {
    struct Struct value = {42};
    return value;
}

int main() {
    return make_struct().value;
}
