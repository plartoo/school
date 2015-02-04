/*
 * Authors: Ian Perera and Phyo Thiha
 * CSC456/Assignment 4
 * 
 * Description: Test file for implementation of mutex lock for kernel space.
 * 		  Refer to README for explanation of what we're doing here.
 * 
 * Usage (to be used in QEMU's user space): 
 *      $ gcc -o sync_test sync_test.c
 * 	 $ ./sync_test
 * 
 */

#include <stdio.h>
#include <linux/unistd.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <stdlib.h>

#define _DOEVENTOPEN_ 329
#define _DOEVENTCLOSE_ 330
#define _DOEVENTWAIT_ 331
#define _DOEVENTSIG_ 332
#define _DOEVENT_INIT_ 333

int main(int argc, char *argv[])
{
        int eventId = 0;
        int eventId2 = 0;
        int eventId3 = 0;
        syscall(_DOEVENT_INIT_);

        eventId = syscall(_DOEVENTOPEN_);
        eventId2 = syscall(_DOEVENTOPEN_);
        eventId3 = syscall(_DOEVENTOPEN_);
        pid_t pID = fork();
        if (pID == 0)
        {
                // Testing signal with no processes waiting
                printf("1.\tProcees %d sleeping\n", getpid());
                sleep(2);

                // Testing signal with a process waiting
                printf("2.\tProcess %d going to wait\n", getpid());
                syscall(_DOEVENTWAIT_,eventId);
                printf("3.\tProcess %d done waiting\n", getpid());

                sleep(2);
                // Testing signal with multiple processes waiting
                fork();
                printf("4.\tProcess %d going to wait\n", getpid());
                syscall(_DOEVENTWAIT_,eventId2);
                printf("5.\tProcess %d done waiting\n", getpid());
                printf("6.\tProcess %d going to wait\n", getpid());
                syscall(_DOEVENTWAIT_,eventId3);
        }
        else if (pID < 0)
        {
                printf("Error forking\n");

                exit(1);
        }
        else
        {
                printf("7.\tProcess %d going to signal before child\n", getpid());
                printf("8.\tProcesses released: %d\n",
                                syscall(_DOEVENTSIG_,eventId));
                printf("9.\tProcess %d going to signal after child\n", getpid());
                sleep(3);
                printf("10.\tProcesses released: %d\n",
                                syscall(_DOEVENTSIG_,eventId));
                sleep(3);
                printf("11.\tProcess %d going to signal after two children\n",
                                getpid());
                printf("12.\tProcesses released: %d\n",
                                syscall(_DOEVENTSIG_,eventId));

                sleep(2);
                syscall(_DOEVENTCLOSE_,eventId);
                syscall(_DOEVENTCLOSE_,eventId2);
                syscall(_DOEVENTCLOSE_,eventId3);
        }
        return 0;
}
