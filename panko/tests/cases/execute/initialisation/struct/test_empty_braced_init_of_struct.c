int memcmp(void const*, void const*, typeof(sizeof 0));

struct T {
    int x;
    char cs[4];
    int y;
};

int main() {
    struct T zeroed_t = {};
    char zeros[sizeof zeroed_t] = {};
    return memcmp(&zeroed_t, zeros, sizeof zeroed_t);
}
