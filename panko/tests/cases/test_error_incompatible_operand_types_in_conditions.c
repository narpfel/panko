void void_function() {}

int* ptr_function() {}

int main() {
    1 ? void_function() : 42;
    2 ? ptr_function() : 42;
    3 ? 27 : void_function();
    4 ? 27 : ptr_function();
}
