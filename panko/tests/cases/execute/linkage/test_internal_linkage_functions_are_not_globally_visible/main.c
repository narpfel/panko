int puts(char const*);

void f();
void g();

void function() {
    puts("function");
}

int main() {
    // [[print: f]]
    f();
    // [[print: g]]
    g();
    // [[print: function]]
    function();
}
