struct Struct {
    int member;
};

struct Struct make_struct();

int main() {
    make_struct().member = 42;
    struct Struct value = {};
    (true ? value : value).member = 27;
}
