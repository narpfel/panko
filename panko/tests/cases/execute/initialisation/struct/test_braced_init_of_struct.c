int memcmp(void const*, void const*, typeof(sizeof 0));

struct T {
    int x;
    char cs[4];
    int y;
    int z;
};

int main() {
    struct T zeroed_t = {0x1234'5678, 1, 2, 3, 4, 0xabcd'ef42};
    char bytes[sizeof zeroed_t] = {
        0x78, 0x56, 0x34, 0x12,
        1, 2, 3, 4,
        0x42, 0xef, 0xcd, 0xab,
        0, 0, 0, 0,
    };
    return memcmp(&zeroed_t, bytes, sizeof zeroed_t);
}
