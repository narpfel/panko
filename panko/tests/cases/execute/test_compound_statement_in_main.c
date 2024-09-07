// [[return: 42]]

int main() {
    int a = 42;
    {
        int a = 27;
    }
    return a;
}
