// [[return: 42]]

int main() {
    char x = 42;
    char* p = &x;
    char** pp = &p;
    return **pp;
}
