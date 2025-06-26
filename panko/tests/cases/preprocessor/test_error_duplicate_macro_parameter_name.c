#define MACRO(abc, def, abc) abc + def

int main() {
    return MACRO(1, 2, 3);
}
