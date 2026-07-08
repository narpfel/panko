// [[known-bug: pointers to incomplete structs are not completed when the struct is completed]]
// [[return: 4]]

int printf(char const*, ...);

struct T* ptrs[10];

struct T {
    int x;
};

struct T* ptr;

int main() {
    // [[print: 1]]
    printf("%d\n", ptrs[0] == ptr);
    return sizeof *ptrs[0];
}
