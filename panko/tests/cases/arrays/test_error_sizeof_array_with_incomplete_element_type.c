int main() {
    sizeof(typeof(int[])[0]);
    sizeof(int[1][]);
    sizeof(struct T[10]);
    sizeof(typeof(void[42]));
    sizeof(void[42]);
}
