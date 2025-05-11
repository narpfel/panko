typedef int global;
int global;

int another_global;
typedef int another_global;

int main() {
    typedef int local;
    int local;

    int another_local;
    typedef int another_local;
}
