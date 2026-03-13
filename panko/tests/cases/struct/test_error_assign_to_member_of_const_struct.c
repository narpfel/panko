struct Struct {
    int x;
    int const y;
    int z;
};

int main() {
    struct Struct const value = {1, 2, 3};
    value.x = 42;
    value.y = 123;
    value.z = 27;
}
