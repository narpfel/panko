// [[preprocessor-only]]

#define another(t) LONG(t)

#define LONG(type) another(long) LONG(type)

int main() {
    LONG(another(int)) x = 42;
}
