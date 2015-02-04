#include <linux/kernel.h>
#include <linux/prinfo.h>
#include <linux/sched.h> // task_struct
#include <linux/list.h>  // list_for_each
#include <asm/current.h> // get current process to find ours


/* Estimating nice values from priority. Got the idea from an online book.
   http://book.chinaunix.net/special/ebook/PrenticeHall/PrenticeHallPTRTheLinuxKernelPrimer/0131181637/ch03lev1sec2.html 
*/
/*
 * Macros to convert between static_priority and nice value
 */
#define NICE_TO_PRIO(nice)	(MAX_RT_PRIO + (nice) + 20)
#define PRIO_TO_NICE(prio)	((prio) - MAX_RT_PRIO - 20)
//
#define COMMAND_LEN	16
#define DEBUG		1

#define MAXLONG 2147483647*2147483647

// collect youngest child's pid, children's total user and system times
/*
 * The input is the pointer *task (of task_struct type)
 * The other arguments are empty pointers which should be filled in by the function
 */
void get_children_info(struct task_struct *task, pid_t *ychld_pid, long *cutime, long *cstime)
{
  struct task_struct *curtask;
  struct list_head *list;

  long max_start_time = -1;
  // initialize the times to zero
  
  *cutime = 0;
  *cstime = 0;
  
  list_for_each(list, &task->children)
    {
      // get the next child
      curtask = list_entry(list, struct task_struct, sibling);
      // check whether the child is younger
      if(curtask->start_time.tv_nsec > max_start_time)
	{
	  max_start_time = curtask->start_time.tv_nsec;
	  *ychld_pid = curtask->pid;
	}
      // keep adding the user and system times
      *cutime += curtask->utime;
      *cstime += curtask->stime;
    }
}

void get_sibling_info(struct task_struct *task, struct list_head *list, pid_t *ys_pid, pid_t *os_pid)
{

  struct task_struct *current_sibling;

  long ys_time = MAXLONG;
  long os_time = -1;
  *ys_pid = -1;
  *os_pid = -1;

  int flag_s = 0;
  //int flag_o = 0;

   //printk("Inside the function sibling\n");

  // ref. http://lxr.linux.no/#linux+v3.0.4/include/linux/list.h

  list_for_each(list, &task->sibling)
    {
      current_sibling = list_entry(list, struct task_struct, sibling);

      //printk("pid = %d, start time = %ld, current start time  = %ld\n", current_sibling->pid, current_sibling->real_start_time.tv_sec, task->real_start_time.tv_sec);

      if(current_sibling->pid != 0)
	{ 
	  // check if iterates back to the current process
	  if(current_sibling->pid == task->pid)
	    {
	      //printk("=======================\n");
	      //printk("Back to the same task\n");
	      break;
	    }

	  if(current_sibling->real_start_time.tv_sec >= task->real_start_time.tv_sec)
	    {
	      //printk("younger\n");
	      //printk("ystime = %ld\n", ys_time);

	      // the sibling process is younger than given process
		if(flag_s == 0)
		{
		  flag_s = 1;
		  //printk("Found younger sibling %d\n", current_sibling->pid);
		  ys_time = current_sibling->real_start_time.tv_sec;
		  *ys_pid = current_sibling->pid;
		}
	       else if(current_sibling->real_start_time.tv_sec < ys_time)
		{
		  //printk("Found younger sibling %d\n", current_sibling->pid);

		  ys_time = current_sibling->real_start_time.tv_sec;
		  *ys_pid = current_sibling->pid;
		}	  
	    }

	  // look for older sibling
      if(current_sibling->real_start_time.tv_sec <= task->real_start_time.tv_sec)
	{
	  //printk("older\n");
	  //printk("previous ostime = %ld\n", os_time);
	  // check whether it is the youngest among older processes
	  if( current_sibling->real_start_time.tv_sec  >= os_time )
	    {
	      //printk("Found older sibling %d\n", current_sibling->pid);
	      os_time =  current_sibling->real_start_time.tv_sec;
	      *os_pid = current_sibling->pid;
	    }
	}

	}
    }
  //printk("outside the loop\n");

  //printk("Younger sibling: %d\n", *ys_pid);
  //printk("Older sibling: %d\n", *os_pid);

  
}

asmlinkage long sys_prinfo(struct prinfo *info)
{
  struct task_struct *task;
  struct list_head *list;
  struct task_struct * current_sibling;


  // sanity checking
  if(info == NULL)
    {
      printk("Unexpected NULL input receieved. Exit the System Call.\n");      
      return (long) (EINVAL); // what is EINVAL? Need to ask Phyo.
    }

  strcpy(info->comm, "\0");
  // initialize the younger and older sibling pid values
  //pid_t ys_pid = -1;
  //pid_t os_pid = -1;

  //
  info->youngest_child_pid = -1;
  info->younger_sibling_pid = -1;
  info->older_sibling_pid = -1;
  //
  info->cutime = 0;
  info->cstime = 0;



  //iterate through the list of all processes  
  for_each_process(task)
  {
    //check if the pid values match
    if (task->pid == info->pid){
      // yes, pid matched
      // read the state value
      info->state = task->state;
      // get the nice value from static priority
      info->nice = PRIO_TO_NICE(task->static_prio);
      // copy the pid values (although, we expect them to be the same)
      // kind of unnecessary thing to do. May delete if all agree
      info->pid = task->pid;
      
      // could happen if the process is init??
      if(task->parent != NULL){
	info->parent_pid = task->parent->pid;
      }
      else{
	info->parent_pid = -1;
      }

      // ref. http://lxr.linux.no/#linux+v3.0.4/include/linux/list.h
      
      list_for_each(list, &task->sibling)
	{
	  printk("Searching for younger siblings\n");
	  current_sibling = list_entry(list, struct task_struct, sibling);
	  if(current_sibling!=NULL)
	    {
	      info->younger_sibling_pid = current_sibling->pid;
	    }
	  else
	    {
	      info->younger_sibling_pid = -1;
	    }
	  break;
	}
      
      list_for_each_prev(list, &task->sibling){
	printk("Searching for the older siblings\n");
	current_sibling = list_entry(list, struct task_struct, sibling);
	if(current_sibling!= NULL)
	  {
	    info->older_sibling_pid = current_sibling->pid;
	  }
	else
	  {
	    info->older_sibling_pid = -1;
	  }
	break;
      }

      info->start_time = task->start_time.tv_sec;
      info->user_time = task->utime;
      info->sys_time = task->stime;

      // get all the children related info. Get the youngest child pid, childrens' user and system time
      get_children_info(task, &info->youngest_child_pid, &info->cutime, &info->cstime);
      
      get_sibling_info(task, list, &info->younger_sibling_pid, &info->older_sibling_pid);

      printk("siblings: %d %d \n", info->younger_sibling_pid, info->older_sibling_pid);

      // get the uid of the taks
      info->uid = task->uid;
      // get the command info copied to the stuct
      strncpy(info->comm, task->comm, COMMAND_LEN);

      if (DEBUG==1){
	printk("\nCurrent process\n=============\n");
	printk("state:\t%ld\n", info->state);
	printk("nice value:\t%ld\n", info->nice);
	printk("pid:\t%d\n", info->pid);
	printk("parent pid:\t%d\n", info->parent_pid);
	printk("older_sibling_pid:\t%d\n", info->older_sibling_pid);
	printk("younger_sibling_pid:\t%d\n", info->younger_sibling_pid);
	printk("parent pid:\t%d\n", info->parent_pid);
	printk("process start time:\t%lu\n", info->start_time);
	printk("user time: %ld\n", info->user_time);
	printk("sys time: %ld\n", info->sys_time);
	
	printk("youngest_child_pid:\t%d\n", info->youngest_child_pid);
	printk("cutime: %ld\n", info->cutime);
	printk("cstime: %ld\n", info->cstime);
	
	//printk("command = %s\n", info->comm);
	printk("program executed: %s\n", info->comm);
	printk("------------\n");
      }
    }
  }

  return (long) 0;
}



