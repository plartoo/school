#include <stdio.h>
#include <linux/unistd.h>
#include <sys/syscall.h> 
#include "prinfo.h"
#include <stdlib.h>
#include <string.h>

//#define _CS2456_TEST_ 327
#define _PRINFO_TEST_ 328

int main(int argc, char *argv[])
{ 
  struct prinfo info;
  struct prinfo *ptr_info;
  char state_string[32];
  
  
  if(argc<2)
  {
    ptr_info = NULL;
  }
  else if(argc==2)
  {
    pid_t input_pid = atoi(argv[1]);
     //info.pid = getpid();
     info.pid = input_pid;
     printf("The pid = %d\n", info.pid);
     ptr_info = &info;     
  }
  else
  {
    printf("Usage: ./test_prinfo <pid>\n");
     exit(EXIT_FAILURE);
  } 
    
  long var = syscall(_PRINFO_TEST_, ptr_info);
  
  if (var == (long)(EINVAL))
    return 0;
  
  if(strcmp(ptr_info->comm, "\0") != 0)
  {
  
  printf("======= Process Information =======\n");
  printf("process name: %s\n", info.comm );
  printf("process nice: %ld\n", info.nice);
    
  if(info.state==0)
  {
    strcpy(state_string, "TASK_RUNNING");
  }    
  else if(info.state==1)
  {
    strcpy(state_string, "TASK_INTERRUPTIBLE");    
  }
  else if(info.state==2)
  {
  strcpy(state_string, "TASK_UNINTERRUPTIBLE");
  }
  else if(info.state==4)
  {
  strcpy(state_string, "TASK_ZOMBIE");    
  }
  else if(info.state==8)
  {
  strcpy(state_string, "TASK_STOPPED");
  }
  else if(info.state==32)
  {
  strcpy(state_string, "TASK_EXCLUSIVE");
  }
  else if(info.state==132)
  {
    strcpy(state_string, "TASK_SUSPENDED");
  }
  
  
     printf("process state: %ld [%s]\n", info.state, state_string);
     printf("process pid: %d\n", info.pid);
     printf("parent pid: %d\n", info.parent_pid);
     printf("youngest child pid: %d\n", info.youngest_child_pid);
     printf("younger sibling pid: %d\n", info.younger_sibling_pid);
     printf("older sibling pid: %d\n", info.older_sibling_pid);
     printf("start time: %lu\n", info.start_time);
     printf("user time: %ld\n", info.user_time);
     printf("system time: %ld\n", info.sys_time);
     printf("children user time (cutime): %ld\n", info.cutime);
     printf("children system time (cstime): %ld\n", info.cstime);
     printf("user id: %ld\n", info.uid);     
    
  }  


 
  printf("\nRising to user level\n\n");
     
  return 0;
}

