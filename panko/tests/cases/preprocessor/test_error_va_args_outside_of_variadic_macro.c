#define MACRO(x, y) __VA_ARGS__ - x + y

int main() {
    return MACRO(4, 7);
}
