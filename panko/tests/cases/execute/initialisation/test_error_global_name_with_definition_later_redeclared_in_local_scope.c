// [[return: 42]]
// [[known-bug: codegen incorrectly assumes that `extern`al variable declarations in local scope are tentative definitions]]

int name = 42;

int main() {
    int name = 27;
    {
        extern int name;
        return name;
    }
}
