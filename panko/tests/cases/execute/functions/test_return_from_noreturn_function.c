// [[signal: SIGILL]]

// [[stderr-re: ^.*Error:.* control flow returned from function `.*g.*` declared `.*_Noreturn.*`\n]]

_Noreturn void g() {
    return;
}

int main() {
    g();
}
