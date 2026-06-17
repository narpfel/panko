// [[return: 42]]
// [[known-bug]]

int name = 42;

int main() {
    int name = 27;
    {
        extern int name;
        return name;
    }
}
