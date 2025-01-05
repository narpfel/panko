void zero_params() {}
void one_param(int) {}
void two_params(int, int) {}

void (*returns_function_pointer())(int, int) {
    return two_params;
}

void varargs(int, int, ...) {}

int main() {
    zero_params(1);
    one_param();
    two_params(42);
    returns_function_pointer()();
    varargs(42);
}
