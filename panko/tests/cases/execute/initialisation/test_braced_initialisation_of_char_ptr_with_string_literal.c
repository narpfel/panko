// [[known-bug]]
// [[return: 3]]

typeof(sizeof 0) strlen(char const*);

int main() {
    char const* s = {"abc"};
    return strlen(s);
}
