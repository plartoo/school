#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <linux/unistd.h>
#include <sys/syscall.h>
#include <linux/sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <linux/unistd.h>
#include <sys/syscall.h>
#include <linux/sched.h>
#include <sched.h>
#define _GET_GROUP_WEIGHT 334
#define _SET_GROUP_WEIGHT 335

unsigned long long get_total_cpu_usage(){

        FILE *ptr_file;
        unsigned long long user, nice, system, idle, iowait, irq,
                 softirq, steal, total;

        ptr_file = fopen("/proc/stat", "r");
        if (!ptr_file)
                return 1;
        fscanf(ptr_file, "cpu  %llu %llu %llu %llu %llu %llu %llu %llu",
                &user, &nice, &system, &idle, &iowait, &irq, &softirq,
                &steal);
        fclose(ptr_file);

        total = user + nice + system + idle + iowait + irq + softirq + steal;
        return total;

}

unsigned long long get_process_cpu_usage(int process_id){
        FILE *ptr_file;
        int pid;
        char comm[100];
        char c;
        int ppid;
        int pgrp;
        int session;
        int tty_nr;
        int tpgid;
        unsigned int flags;
        unsigned long minflt, cminflt, majflt, cmajflt, utime, stime;

        char pid_buffer[20];
        int i = sprintf(pid_buffer, "%d", process_id);
        char str[32];
        strcpy(str, "/proc/");
        strcat(str, &pid_buffer[0]);
        strcat(str, "/stat");
        //printf("command: %s\n", str);

        ptr_file = fopen(str, "r");
        if (!ptr_file)
                return 1;

        fscanf(ptr_file, "%d %s %c %d %d %d %d %d %u %lu %lu %lu %lu %lu %lu",
                &pid, &comm, &c, &ppid, &pgrp, &session, &tty_nr, &tpgid,
                &flags, &minflt, &cminflt, &majflt, &cmajflt, &utime, &stime);
        fclose(ptr_file);
        return (utime + stime);

}

int main(int argc, char *argv[])
{
        int i, j;
        pid_t pid;
        unsigned long long total_before, total_after, total_diff;
        unsigned long long process_before, process_after, process_diff;
        double usage = 0.0;

        if(argc == 1) {
                pid = getpid();
                printf("\nUsing my own pid: %d\n************************\n\n",
                        pid);
        }
        else if (argc == 2) {
                pid = (pid_t) atoi(argv[1]);
                printf("\nUsing other's pid: %d\n************************\n\n",
                        pid);
        }
        else {
                printf( "\nUsage: %s [optional_pid]\n\n", argv[0] );
        }

        // get uid and gid stuff
        uid_t uid = getuid();
        uid_t euid = geteuid();
        gid_t gid = getgid();
        gid_t egid = getegid();
        printf("old => uid: %d\teuid: %d\tgid: %d\tegid: %d\n",
                uid, euid, gid, egid);

        int ret = setgid((gid_t) 1006);
        printf("setgid res: %d\n", ret);
        printf("new gid: %d\n", getgid());

        // group weight and stuff
        int grpwt = syscall(_GET_GROUP_WEIGHT, (int)gid);
        ret = syscall(_SET_GROUP_WEIGHT, (int)gid, 10);
        int new_grpwt = syscall(_GET_GROUP_WEIGHT, (int)gid);
        printf("gid weight => old: %d\tstatus: %d\tnew: %d\n",
                grpwt, ret, new_grpwt);

        j = 0;
        while(1){
                j++;
                printf("%d\n====\n", j);

                total_before = get_total_cpu_usage();
                process_before = get_process_cpu_usage((int) pid);

                for (i=0; i < 100000000; i++){} // use this?


                total_after  = get_total_cpu_usage();
                process_after = get_process_cpu_usage((int) pid);

                total_diff = total_after - total_before;
                process_diff =  process_after - process_before;

                //printf("total_before: %llu\n", total_before);
                //printf("total_after: %llu\n", total_after);
                printf("total_diff: %llu\n", total_diff);
                //printf("\n\n");
                //printf("process_before: %llu\n", process_before);
                //printf("process_after: %llu\n", process_after);
                printf("process_diff: %llu\n", process_diff);
                printf("==>");

                if ((total_diff > 0) && (process_diff > 0)){
                        usage = (usage +
                        (100 * (process_diff / (float) total_diff)))/2.0;
                        printf("percentage of use: %.2f\n\n", usage);
                }
                fflush(stdout);
        }

        return 0;
}



