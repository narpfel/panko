int f();
int* pf();

int main() {
    &42;
    &f();

    // okay
    &*pf();
}
