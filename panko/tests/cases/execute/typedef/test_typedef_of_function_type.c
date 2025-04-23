int puts(char const*);

typedef int FunctionPtr(char const*);

int main() {
    FunctionPtr* ptr = puts;
    // [[print: hello world]]
    ptr("hello world");
}
