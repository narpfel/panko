#define MACRO(x, y) __VA_OPT__(42 -) x + y

int main() {
    return MACRO(1, 2);
}
