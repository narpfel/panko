// [[return: 44]]

int plus_1(int x) {
    return x + 1;
}

int f(int callback(int));

int main() {
    return f(plus_1);
}

int f(int callback(int)) {
    return callback(callback(42));
}
