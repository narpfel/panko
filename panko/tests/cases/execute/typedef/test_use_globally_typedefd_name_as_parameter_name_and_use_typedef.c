// [[return: 42]]

typedef int a;

int f(int a) {
    return a;
}

a global_value = 42;
// ↑ this checks that the next token after the body is lexed after the
// function’s `typedef` scope was popped

int main() {
    return f(global_value);
}
