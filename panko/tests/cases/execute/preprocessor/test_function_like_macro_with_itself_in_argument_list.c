// [[return: 28]]

#define LONG(type) long type
#define ADD_1(expr) expr + 1

int main() {
    LONG(LONG(int)) x = 42;
    return _Generic(x, long long: ADD_1(27));
}
