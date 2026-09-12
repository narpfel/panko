// [[return: 8]]

struct T {
    int x;
};

int main() {
    struct T {
        long l;
    };
    return sizeof(struct T);
}
