// [[known-bug]]
// [[compile-error: functions must have a body, not an initialiser]]
// [[nosnapshot]]

int main() {
    int f() = {};
}
