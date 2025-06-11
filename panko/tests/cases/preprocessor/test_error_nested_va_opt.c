#define MACRO(x, y, ...) __VA_OPT__(42 __VA_OPT__(-)) x + y

int main() {
    return MACRO(1, 2, name);
}
