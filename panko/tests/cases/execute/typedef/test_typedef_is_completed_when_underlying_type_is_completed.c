// [[known-bug: type aliases of incomplete struct type are not completed when the struct is completed]]
// [[return: 4]]

int printf(char const*, ...);

typedef struct T TypedefT;

struct T {
    int x;
};

int main() {
    TypedefT value = { .x = 4 };
    return value.x;
}
