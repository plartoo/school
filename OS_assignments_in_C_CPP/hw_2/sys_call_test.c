#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <linux/unistd.h>
#include <sys/syscall.h>
#include <unistd.h>

#define _GET_GROUP_WEIGHT 334
#define _SET_GROUP_WEIGHT 335

int main(int argc, char *argv[])
{
  gid_t gid = getgid();
  printf("gid: %d\n", gid);

  uid_t uid = getuid();
  printf("uid: %d\n", uid);

  printf("\nDiving to kernel level\n\n");
  gid_t old_wt = syscall(_GET_GROUP_WEIGHT, gid);
  int res = syscall(_SET_GROUP_WEIGHT, gid, 30);
  gid_t new_wt = syscall(_GET_GROUP_WEIGHT, gid);

  printf("\nRising to user level\n");
  printf("\nold_wt: %d\tsuccess:%d\tnew_wt: %d\n", old_wt, res, new_wt);

  return 0;
}
