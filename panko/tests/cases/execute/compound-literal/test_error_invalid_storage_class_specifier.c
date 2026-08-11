// [[compile-error: invalid storage class `extern` applied to compound literal]]

int a = (extern int){42};
int b = (typedef int){27};

// okay
int c = (static int){};

int main() {
    (extern int){1};
    (typedef int){2};

    // okay
    (static int){3};
}
