#include <linux/kernel.h>
#include <linux/prinfo.h>
#include <linux/sched.h> // task_struct
#include <linux/list.h>  // list_for_each
#include <asm/current.h> // get current process to find ours

/*
 * Convert user-nice values [ -20 ... 0 ... 19 ]
 * to static priority [ MAX_RT_PRIO..MAX_PRIO-1 ],
 * and back.
 */
#define NICE_TO_PRIO(nice)	(MAX_RT_PRIO + (nice) + 20)
#define PRIO_TO_NICE(prio)	((prio) - MAX_RT_PRIO - 20)
#define TASK_NICE(p)		PRIO_TO_NICE((p)->static_prio)
#define COMMAND_LEN	16
#define DEBUG		1

// collect youngest child's pid, children's total user and system times
void get_children_info(struct task_struct *task, pid_t *ychld_pid, long *cutime, long *cstime)
{
  struct task_struct *curtask;
  struct list_head *list;

  long max_start_time = 0;

  list_for_each(list, &task->children)
    {
      curtask = list_entry(list, struct task_struct, sibling);
      if(curtask->start_time.tv_sec > max_start_time)
	{
	  max_start_time = curtask->start_time.tv_sec;
	  *ychld_pid = curtask->pid;
	}
      *cutime += curtask->utime;
      *cstime += curtask->stime;
    }
}

asmlinkage long sys_prinfo(struct prinfo *info)
{

  struct task_struct *task;
  struct list_head *list;
  struct task_struct * current_sibling;

  info->youngest_child_pid = -1;
  info->cutime = 0;
  info->cstime = 0;

  // sanity checking
  if(info == NULL)
    return (long) (EINVAL);

  for_each_process(task)
  {
	if (task->pid == info->pid){
		info->state = task->state;
		info->nice = PRIO_TO_NICE(task->static_prio);
		info->pid = task->pid;

		// could happen if the process is init??
		if(task->parent != NULL){
			info->parent_pid = task->parent->pid;
		}
		else{
		        info->parent_pid = -1;
		}

		// ref. http://lxr.linux.no/#linux+v3.0.4/include/linux/list.h
		list_for_each(list, &task->sibling){
	        	current_sibling = list_entry(list, struct task_struct, sibling);
		        info->younger_sibling_pid = current_sibling->pid;
		        break;
		}
      
		list_for_each_prev(list, &task->sibling){
	        	current_sibling = list_entry(list, struct task_struct, sibling);
		        info->older_sibling_pid = current_sibling->pid;
		        break;
		}

		info->start_time = task->start_time.tv_sec;
		info->user_time = task->utime;
		info->sys_time = task->stime;

		get_children_info(task, &info->youngest_child_pid, &info->cutime, &info->cstime);
		info->uid = task->uid;
		strncpy(info->comm, task->comm, COMMAND_LEN);

		if (DEBUG==1){
			printk("\nCurrent process\n=============\n");
			printk("state:\t%ld\n", info->state);
	    		printk("nice value:\t%ld\n", info->nice);
			printk("pid:\t%d", info->pid);
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

			printk("uid = %ld\n", info->uid);
			printk("program executed:\t%s\n", info->comm);
			printk("------------\n");
		}
	}
  }

  return (long) 0;
}



