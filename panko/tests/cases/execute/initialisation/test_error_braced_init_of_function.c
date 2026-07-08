// [[known-bug: the error message for this case is slightly suboptimal]]
// [[compile-error: functions must have a body, not an initialiser]]
// [[nosnapshot]]

int main() {
    int f() = {};
}
