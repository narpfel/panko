// [[return: 52]]

#define PASTE(x, y, ...) x ## y ## __VA_OPT__(3) ## __VA_OPT__(4) ## __VA_ARGS__ ## __VA_ARGS__

int main() {
    return PASTE(0, x, l);
}
