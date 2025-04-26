int puts(char const*);

void f() {
    puts("f");
}

void g() {
    puts("g");
}

int main() {
    // [[print: f]]
    1 ? f() : g();
}
