union IntInt {
    int i;
    unsigned u;
};

struct T {
    int x;
    int y;
};

union IntStruct {
    int x;
    struct T t;
};

int main() {
    union IntInt int_int = {1, 2};
    union IntStruct int_struct = {1, 2, 3};
}
