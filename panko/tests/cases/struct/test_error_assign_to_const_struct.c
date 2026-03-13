struct Struct {
    int x;
    int y;
    int z;
};

int main() {
    struct Struct const value = {1, 2, 3};
    struct Struct const const_value = value;
    const_value = value;
}
