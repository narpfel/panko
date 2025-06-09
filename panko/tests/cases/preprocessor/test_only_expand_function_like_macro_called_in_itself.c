#define ALSO_LONG(t) LONG(t)
#define LONG(type) ALSO_LONG(long) LONG(type)
#define ADD_1(expr) expr + 1

int main() {
    LONG(LONG(int)) x = 42;
    return _Generic(x, long long: ADD_1(27));
}
