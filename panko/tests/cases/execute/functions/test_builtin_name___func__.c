int printf(char const*, ...);

#define PRINT_FUNC() printf("%s %zu %d\n", __func__, sizeof __func__, __func__ == __func__)

void function() {
    PRINT_FUNC();
}

int main() {
    // [[print: function 9 1]]
    function();

    // [[print: main 5 1]]
    PRINT_FUNC();

    char const* func = __func__;
    // [[print: 1]]
    printf("%d\n", func == __func__);

    typeof(char const[sizeof __func__])* ptr_to_func = &__func__;
    // [[print: 1]]
    printf("%d\n", *ptr_to_func == func);
}
