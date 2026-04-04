struct Struct {
    int x:20;
};

int main() {
    struct Struct s = {};
    int* ptr = &s.x;
}
