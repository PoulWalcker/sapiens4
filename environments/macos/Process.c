#include <sys/wait.h>

/* WNOWAIT reserves the child's PID until owned descendants have been stopped. */
int sapiens_process_exited(pid_t pid) {
    siginfo_t status = {0};
    if (waitid(P_PID, pid, &status, WEXITED | WNOHANG | WNOWAIT) == -1)
        return -1;
    return status.si_pid != 0;
}
