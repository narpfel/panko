int puts(char const*);

int main() {
    typeof(char[])* hello = &"hello";
    // [[print: hello]]
    puts(*hello);
    // [[print: world]]
    puts(&*"world");
}
