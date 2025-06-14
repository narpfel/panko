#define EMPTY_OBJECT
#define EMPTY_FUNCTION()

#define MACRO(...) __VA_OPT__(return 42)

int main() {
    MACRO(EMPTY_OBJECT);
    MACRO(EMPTY_FUNCTION());
}
