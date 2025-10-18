int puts(char const*);

int main() {
    char const* hello_world[] = {"hello", "world", "or something"};
    // [[print: hello]]
    puts(hello_world[0]);
    // [[print: world]]
    puts(hello_world[1]);
    // [[print: or something]]
    puts(hello_world[2]);
}
