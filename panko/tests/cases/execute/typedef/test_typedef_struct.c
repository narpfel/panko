// [[return: 42]]

typedef struct T { int x; } Name;

int main() {
    Name value = {42};
    return value.x;
}
