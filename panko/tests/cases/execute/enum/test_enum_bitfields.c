// [[known-bug: this should zero-extend the bitfield members because `enum E`’s compatible type should be `unsigned`]]
// [[return: 3]]

enum E { A, B, C };

struct T {
    enum E x:2;
    enum E:0;
    enum E y:2;
};

int main() {
    struct T t = { .x = B, .y = C };
    return t.x + t.y;
}
