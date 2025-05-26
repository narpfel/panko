// [[return: 42]]

int puts(char const*);

int main() {
    // [[print: hello world]]
    pu\
ts("hello \
world");

    // [[print: hello world]]
    puts\
("hello world");

    in\
t va\
lue = 42;
    return value;
}
