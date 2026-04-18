struct Struct {
    int x:20;
};

int main() {
    struct Struct s = {};
    int* ptr = &s.x;
}

struct NonintegralBitfield {
    struct Struct bitfield:42;
};

struct TooLarge {
    int too_large_int:123;
    bool b:2;
};

struct ZeroWidthBitfieldWithName {
    int x:2;
    int zero_length:0;
    int y:2;
};
