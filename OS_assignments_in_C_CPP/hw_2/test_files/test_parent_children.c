// Authors: Iftekhar, Phyo and Rahman
// Description:
// spawn a bunch of child processes to test assignment #2's 
// parent/children relationship.
//
// TEST PLAN
//
// test "parent-children" relationship
// 
// run this program by
// $ ./test_parent_children
// then call our syscall program to watch the process parameters
// $ ./prinfo.c <pid_of_the_child_that_we_are_interested_in_checking>
// 
// This will spawn 3 children.  
// The first will quit after 20 seconds and the other two will keep running forever.
// So before that time 20secs, we should do
// $ ./prinfo <parent_pid>
// and we'll see that, the parent process's youngest child will be child #1's pid.
// 
// After 20 secs, we can run again
// $ ./prinfo <parent_pid>
// and see child #2's pid as its youngest kid.
//
// If we want, we can do:
// $ ./prinfo <child_2_pid>
// and we should see its parent pid reported by syscall matches with the original parent's pid.
//


#include <unistd.h>

int main(){
  int pid_1 = -1;
  int youngest_pid = -1;

  if((pid_1 = fork()) == 0){
    sleep(20) ;
    printf("Youngest child exited.\n");
    exit(0);
  }

  printf("parent pid: %d\n", (int)getpid());

  youngest_pid = pid_1;
  printf("child 1 pid: %d\n", pid_1);

  // spawn the rest of children
  if((pid_1 = fork()) == 0){
    while(1);
  }
  printf("child 2 pid: %d\n", pid_1);

  if((pid_1 = fork()) == 0){
    while(1) ;
  }
  printf("child 3 pid: %d\n", pid_1);

  wait(youngest_pid);
  sleep(10);

  printf("Parent exited\n");
  return 0;
}
