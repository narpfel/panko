struct Struct {
    int x;
    int const y;
    int z;
};

int main() {
    struct Struct value = {1, 2, 3};
    struct Struct copy = value;
    value = copy;
}
