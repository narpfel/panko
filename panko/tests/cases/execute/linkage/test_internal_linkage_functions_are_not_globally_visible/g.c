int puts(char const*);

static void function() {
    puts("g");
}

void g() {
    function();
}
