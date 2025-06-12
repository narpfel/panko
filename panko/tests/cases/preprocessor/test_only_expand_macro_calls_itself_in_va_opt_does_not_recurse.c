#define MACRO(x, y, ...) x __VA_OPT__(- MACRO(10, 20, 30))

int main() {
    return MACRO(1, 2, 3);
}
