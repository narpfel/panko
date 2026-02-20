// [[return: 27]]

struct T {
    int x;
    int y;
};

struct T static_struct = {42, 27};

int main() {
    int* p = &static_struct.y;
    return *p;
}
