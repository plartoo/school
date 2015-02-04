/* 
 * Author: Phyo Thiha and Yu Zhong
 * Date: November 15, 2011
 *
 * CSC 456, Assignment 5
 * Description: This is a test program that loops and spends cpu cycle.  The intention was
 * to use this to test our scheduler program, but we ended up proving it other way.
 *
 * To compile and run:
 * $ gcc -o loop loop.c
 * $ ./loop
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <linux/unistd.h>
#include <sys/syscall.h>
#include <linux/sched.h>
#include <sched.h>


int main(int argc, char *argv[])
{

        uid_t uid = getuid();
        uid_t euid = geteuid();
        gid_t gid = getgid();
        gid_t egid = getegid();
        printf("old => uid: %d\teuid: %d\tgid: %d\tegid: %d\n", uid, euid, gid, egid);

        int ustatus = setuid((uid_t) 1001);
        int uestatus = seteuid((uid_t) 1001);

        int gstatus = setgid((gid_t) 1006);
        int gestatus = setegid((gid_t) 1006);
        printf("status => setuid: %d\tseteuid: %d\tsetgid: %d\tsetegid: %d\n", ustatus, uestatus, gstatus, gestatus);

        uid = getuid();
        euid = geteuid();
        gid = getgid();
        egid = getegid();
        printf("new => uid: %d\teuid: %d\tgid: %d\tegid: %d\n", uid, euid, gid, egid);

        pid_t pid = getpid();
        printf("pid: %d\n", pid);

        struct sched_param sp;
        sp.sched_priority = 0;
        //struct sched_param sp = {0};

        int ret = sched_setscheduler(pid, SCHED_GWRR, &sp);
        if (ret == -1) {
                perror("sched_setscheduler");
                return 1;
        }

        while(1){
        int i;
        //for (i=0; i < 100; i++){
                int j;
                for (j=0; j < 100000000; j++){}
                printf("%d\n", i++);
                fflush(stdout);
        }
        return 0;
}

