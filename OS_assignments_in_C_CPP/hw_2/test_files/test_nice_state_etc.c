// Authors: Iftekhar, Phyo and Rahman
// Description: Program to test these two conditions
// a. NULL pointer passed in as "struct prinfo"
// b. Actual pid passed in as part of "struct prinfo"
// 
// TEST PLANS
//
// For condition 'a', we can compile and run this program by:
// $ gcc cs2456_test.c -o cs2456_test
// $ ./cs2456_test
// This should NOT crash the syscall and exit the program gracefully
// with return value EINVAL.
// (specification here <http://www.cs.rochester.edu/users/faculty/sandhya/csc256/>)
//
// For condition 'b', we must have a running program first.
// $ top&
// Note the pid value returned by above command.  Then compile and run our program:
// $ ./cs2456_test <pid_that_we_noted_above>
// This should print out the various attributes of the 'top' process.
//
// testing "process state"
// Read this http://www.linux-tutorial.info/modules.php?name=MContent&pageid=84
// or you can skim over Chapter 4 of the Linux Kernel Development by Love to know
// the possible process states.  My guess is that the "top" process above will have
// state value of "1".
//
// testing "nice value"
// Compare the values from syscall by doing this.
// $ ps -o pid,comm,nice -p <pid_of_top_process_above>
// This will print the nice value of the above pid (that is, "top" process.
// COMPARE that with the one printed out from our syscall
// Then type:
// $ renice 10 -p <pid_of_top_process>
// This will change the nice value of "top" process.  
// Check to make sure it's the same as what's reported by our syscall.
//
// testing current process' "pid" and process' "command"
// duh. (very obvious. this number and string has to match with what we know about "top" process above)
//
// testing "start_time", "user_time", "sys_time"
// These should be obvoius as well.  We can check these against "ps ....." results if we want to.
//
// testing "cutime", "cstime"
// I have no idea how to.  We need to come up with an idea to test them
//
// testing "uid"
// easy breezey.  Just type
// $ id
// that will give you the user id, group id etc. and uid should match what's reported by our syscall.
//


#include <stdio.h>
#include <linux/unistd.h>
#include <sys/syscall.h>
#include <linux/prinfo.h>

#define _PRINFO_ 327

int main(int argc, char *argv[])
{
  struct prinfo my_prinfo;
  struct prinfo *ptr;
  ptr = &my_prinfo;
  
  if (argc < 2){
      ptr = NULL;
  }
  else if (argc == 2){
      ptr->pid = (pid_t)atoi(argv[1]);
  }
  else{
	printf("Usage: ./test_prinfo <pid>\n");
	exit(EXIT_FAILURE);
  }

  printf("\nDiving to kernel level\n\n");
  syscall(_PRINFO_ , ptr);
  printf("\nRising to user level\n\n");

  if(ptr->comm != NULL){
    printf("process name (my_prinfo): %s\n", my_prinfo.comm);
    printf("process nice: %ld\n", my_prinfo.nice);
    printf("process state: %ld\n", my_prinfo.state);
    printf("parent pid: %d\n", my_prinfo.parent_pid);
    printf("youngest child pid: %d\n", my_prinfo.youngest_child_pid);
    printf("younger sib pid: %d\n", my_prinfo.younger_sibling_pid);
    printf("older sib pid: %d\n", my_prinfo.older_sibling_pid);
    printf("start time: %lu\n", my_prinfo.start_time);
    printf("user time: %ld\n", my_prinfo.user_time);
    printf("sys time: %ld\n", my_prinfo.sys_time);
    printf("cutime: %ld\n", my_prinfo.cutime);
    printf("cstime: %ld\n", my_prinfo.cstime);
    printf("user id: %ld\n", my_prinfo.uid);
  }

  return 0;
}

