int printf(char const*, ...);

typedef int name;
typedef int ptr;
typedef int fptr;
typedef int xs;

int main() {
    typedef int function;
    {
        int a, name = 42;
        int b, *ptr = &name;
        int c, function(int);
        int d, (*fptr)(int) = function;
        int e, xs[5] = {123};
        // [[print: 42]]
        printf("%d\n", name);
        ++name;
        // [[print: 43]]
        printf("%d\n", *ptr);
        // [[print: function 27]]
        // [[print: 28]]
        printf("%d\n", function(27));
        // [[print: function 5]]
        // [[print: 6]]
        printf("%d\n", fptr(5));
        // [[print: 123]]
        printf("%d\n", xs[0]);
    }
}

int function(int value) {
    printf("function %d\n", value);
    return value + 1;
}
