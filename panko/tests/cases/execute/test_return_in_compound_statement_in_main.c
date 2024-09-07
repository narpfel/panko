// [[return: 27]]

int main() {
    int a = 42;
    {
        int a = 27;
        return a;
    }
    return a;
}
