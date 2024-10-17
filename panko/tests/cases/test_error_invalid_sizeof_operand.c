void f();

int main() {
    sizeof main;
    sizeof(void);
    sizeof f();
    sizeof(int());

    alignof(void);
    alignof(int());
}
