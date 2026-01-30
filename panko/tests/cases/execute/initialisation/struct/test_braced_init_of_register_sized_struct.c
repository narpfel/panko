// [[known-bug]]
// [[nosnapshot]]

int memcmp(void const*, void const*, typeof(sizeof 0));

struct T {
    char c1;
    char c2;
};

int main() {
    struct T t = {'a', 'b'};
    char bytes[sizeof t] = {'a', 'b'};
    return memcmp(&t, bytes, sizeof t);
}
