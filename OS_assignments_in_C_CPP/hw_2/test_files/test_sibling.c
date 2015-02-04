// Authors: Iftekhar, Phyo and Rahman
// Description:
// spawn a bunch of child processes to test assignment #2's 
// older/younger_sibling.
//
// TEST PLAN
//
// "younger/older" sibling
// 
// run this program by
// $ ./test_sibling
// then call our syscall program to watch the process parameters
// $ ./prinfo.c <pid_of_the_child_that_we_are_interested_in_checking>
// 
// This will spawn 4 children.  But the second child quits 
// earlier than the rest of them.  So before the second child quits,
// we should see child #2 pid as younger sibling of child #1 and
// older sibling of child #3.  If we wait for 10 more seconds,
// the second child will have quit, and child #3 will become the younger
// sibling of child #1.
//

#include <unistd.h>

int main(){
  int pid1 = -1;
  int pid2 = -1;
  int pid3 = -1;
  int pid4 = -1;

  if((pid1 = fork()) == 0){
    sleep(30) ;
    printf("Child 1 exited.\n");
    exit(0);
  }
  printf("Child 1 pid: %d\n", pid0);

  if((pid2 = fork()) == 0){
    sleep(10) ;
    printf("Child 2 exited.\n");
    exit(0);
  }
  printf("Child 2 pid: %d\n", pid1);

  if((pid3 = fork()) == 0){
    sleep(30) ;
    printf("Child 3 exited.\n");
    exit(0);
  }
  printf("Child 3 pid: %d\n", pid2);

  if((pid4 = fork()) == 0){
    sleep(30) ;
    printf("Child 4 exited.\n");
    exit(0);
  }
  printf("Child 4 pid: %d\n", pid3);

  wait(pid1);
  wait(pid2);
  wait(pid3);
  wait(pid4);
  sleep(5);

  printf("Parent process exiting\n");
  return 0;
}

