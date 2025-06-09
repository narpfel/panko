// [[return: 42]]

#define FUNCTION(a, b, c) a - b - c

int main() {
    return FUNCTION(,,) 42;
}
