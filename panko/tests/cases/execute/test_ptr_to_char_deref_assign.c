// [[return: 27]]

int main() {
    char x = 42;
    char* p = &x;
    char** pp = &p;
    **pp = 27;
    return x;
}
