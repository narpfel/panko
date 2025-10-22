// [[signal: SIGILL]]

// TODO: use proper includes (`<signal.h>` and `<unistd.h>`)
#define SIGILL 4
typedef int pid_t;

pid_t getpid();
int kill(pid_t, int);

int main() {
    kill(getpid(), SIGILL);
}
