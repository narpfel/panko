// [[return: 85]]

#define LONG(type) long type
#define ADD_1(expr) expr + 1

int main() {
    LONG(int) x = 42;
    return ADD_1(2 * x);
}
