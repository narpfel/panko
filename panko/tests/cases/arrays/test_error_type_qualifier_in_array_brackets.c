int main() {
    int xs[const] = {1, 2, 3};
}

void qualifier_not_in_outermost_derivation_typeof(typeof(int[const])* xs);
void qualifier_not_in_outermost_derivation(int (*xs)[const]);
