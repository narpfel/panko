int puts(char const*);

static void function() {
    puts("f");
}

void f() {
    function();
}
