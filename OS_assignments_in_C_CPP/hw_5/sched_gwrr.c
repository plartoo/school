/*
 * GWRR Scheduling Class 
 */

#include <linux/kernel.h>

#define GWRR_INIT_PROC if(init == 0) init_gwrr();

struct gwrr_se {
	struct task_struct * p;
	struct gwrr_se * next;
	struct gwrr_se * prev;
};

struct gwrr_group {
	gid_t gid;
	int used;
	int weight;
	int time_left;
	int proc_count;
	struct gwrr_se * rq_head;
	struct gwrr_se * rq_tail;
};

struct gwrr_group group_list[100]; // Basic Hash table
int curr_group = 0;
int init = 0;
int output = 0;

int get_hash(int p) {
	int ret;
	if((group_list[p % 97].used != 0) && (group_list[p % 97].gid != ((gid_t) p))) { // Hash collision
		ret = get_hash(p + 23); // Re-hash
	} else { // No collision
		ret = p % 97;
		group_list[p % 97].used = 1;
		group_list[p % 97].gid = ((gid_t) p);
	}
	return ret;
}

static void init_gwrr(void) {
	printk("gwrr init\n");
	int i;
	for(i = 0; i < 100; i++) {
		group_list[i].used = 0;
		group_list[i].gid = i;
		group_list[i].weight = 10;
		group_list[i].time_left = 10;
		group_list[i].proc_count = 0;
		group_list[i].rq_head = NULL;
		group_list[i].rq_tail = NULL;
	}
	init = 1;
}

asmlinkage int sys_get_group_weight(gid_t gid) {
	GWRR_INIT_PROC
	int gid_hash = get_hash(gid);
	return (group_list[gid_hash].weight);
}

asmlinkage void sys_set_group_weight(gid_t gid, int weight) {
	GWRR_INIT_PROC
	int gid_hash = get_hash(gid);
	group_list[gid_hash].weight = weight;
	return 0;
}

/*
 * Adding/removing a task to/from a priority array:
 */
static void enqueue_task_gwrr(struct rq *rq, struct task_struct *p, int wakeup)
{
	GWRR_INIT_PROC
	//printk("enqueue %d\n", p->pid);
	gid_t gid = get_hash(p->gid);
	
	output = 1;
	// Set up current process's se
	struct gwrr_se * curr_se = kmalloc(sizeof(struct gwrr_se), GFP_KERNEL);
	curr_se->p = p;
	
	struct gwrr_se* test_se = group_list[gid].rq_head;
	while(test_se != NULL) {
		if(test_se->p == p) {
			//printk("already in queue\n");
			return;
		}
		test_se = test_se->next;
	}

	// Put current se into group's rq
	if(group_list[gid].proc_count == 0) { // Empty list
		group_list[gid].rq_head = curr_se;
		group_list[gid].rq_tail = curr_se;
		curr_se->next = NULL;
		curr_se->prev = NULL;
		//printk("enqueue empty, pid = %d, gid = %d\n", p->pid, get_hash(p->gid));
	} else { // Not empty, add to tail
		(group_list[gid].rq_tail)->next = curr_se;
		curr_se->prev = group_list[gid].rq_tail;
		group_list[gid].rq_tail = curr_se;
		curr_se->next = NULL;
		//printk("enqueue, pid = %d, gid = %d\n", p->pid, get_hash(p->gid));
	}
	++(group_list[gid].proc_count);
	return;
}

static void dequeue_task_gwrr(struct rq *rq, struct task_struct *p, int sleep)
{
	GWRR_INIT_PROC
	
	gid_t gid = get_hash(p->gid);
	if(group_list[gid].proc_count == 0) {
		//printk("no member in this queue\n");
		return;
	}
	struct gwrr_se * curr = group_list[gid].rq_head;
	//printk("dequeue %d \n", p->pid);
	while(curr != NULL) {
		if(curr->p->pid == p->pid) { // That's the task!
			if(curr == group_list[gid].rq_head) { // Head
				group_list[gid].rq_head = curr->next;
			} else if(curr == group_list[gid].rq_tail) { // Tail
				group_list[gid].rq_tail = curr->prev;
			}
			if(curr->prev != NULL)
				curr->prev->next = curr->next;
			if(curr->next != NULL)
				curr->next->prev = curr->prev;
			curr->next = NULL;
			curr->prev = NULL;
			--(group_list[gid].proc_count);
			//printk("removed!\n");
			return;
		} else {
			curr = curr->next;
		}
	}
	//printk("return\n");
	return;
}

/*
 * Put task to the end of the run list with the overhead of dequeue
 * followed by enqueue.
 */

static void requeue_task_gwrr(struct rq *rq, struct task_struct *p)
{
	GWRR_INIT_PROC
	dequeue_task_gwrr(rq, p, 1);
	enqueue_task_gwrr(rq, p, 1);
	return;
}

static void yield_task_gwrr(struct rq *rq)
{
	GWRR_INIT_PROC
	printk("yield, call requeue \n");
	requeue_task_gwrr(rq, rq->curr);
}

/*
 * Preempt the current task with a newly woken task if needed:
 */
static void check_preempt_curr_gwrr(struct rq *rq, struct task_struct *p)
{
	GWRR_INIT_PROC
	//printk("check preempt! \n");
	if (p->prio < rq->curr->prio)
		resched_task(rq->curr);
}


static struct task_struct *pick_next_task_gwrr(struct rq *rq)
{
	GWRR_INIT_PROC
	//if(output == 1)
	//	printk("pick_next \n");
	if((group_list[curr_group].time_left <= 0) || (group_list[curr_group].proc_count == 0)) { // It's next group's turn!
		group_list[curr_group].time_left = group_list[curr_group].weight; // Recharge
		int old_group = curr_group;
		curr_group = (curr_group + 1) % 100;
		while(old_group != curr_group) { // Find next group...
			if(group_list[curr_group].proc_count != 0) { // If there's a group rq with a proc to run
				struct task_struct * p = group_list[curr_group].rq_head->p;
				dequeue_task_gwrr(rq, group_list[curr_group].rq_head->p, 1);
				printk("GWRR choose %d \n", p->pid);
				if(p->state < 3)
					return p;
			}
			curr_group = (curr_group + 1) % 100;
		}
	}

	// Pick next task from current group's rq
	struct task_struct * p = NULL;
	if(group_list[curr_group].proc_count != 0) { // No other group need it, choose one from current queue if possible
		p = group_list[curr_group].rq_head->p;
		dequeue_task_gwrr(rq, group_list[curr_group].rq_head->p, 1);
		if(p->state < 3)
			return p;
		else
			p = NULL;
		//printk("choose %d\n", p->pid);
	}
	//printk("finished \n");
	return p;
}

static void put_prev_task_gwrr(struct rq *rq, struct task_struct *p)
{
	//printk("put %d back to %d\n", p->pid, get_hash(p->gid));
	enqueue_task_gwrr(rq, p, 1);
}

/*
 * When switching a task to RT, we may overload the runqueue
 * with RT tasks. In this case we try to push them off to
 * other runqueues.
 */
static void switched_to_gwrr(struct rq *rq, struct task_struct *p,
			   int running)
{
	//printk("switched to 2\n");
	int check_resched = 1;

	/*
	 * If we are already running, then there's nothing
	 * that needs to be done. But if we are not running
	 * we may need to preempt the current running task.
	 * If that current running task is also an RT task
	 * then see if we can move to another run queue.
	 */
	if (!running) {
		if (check_resched && p->prio < rq->curr->prio)
			resched_task(rq->curr);
	}
}

/*
 * Priority of the task has changed. This may cause
 * us to initiate a push or pull.
 */
static void prio_changed_gwrr(struct rq *rq, struct task_struct *p,
			    int oldprio, int running)
{
	if (running) {
		/* For UP simply resched on drop of prio */
		if (oldprio < p->prio)
			resched_task(p);
	} else {
		/*
		 * This task is not running, but if it is
		 * greater than the current running task
		 * then reschedule.
		 */
		if (p->prio < rq->curr->prio)
			resched_task(rq->curr);
	}
}


static void task_tick_gwrr(struct rq *rq, struct task_struct *p, int queued)
{
	--(group_list[get_hash(p->gid)].time_left);
	//if(group_list[get_hash(p->gid)].time_left <= 0)
	resched_task(rq->curr);
}

static void set_curr_task_gwrr(struct rq *rq)
{
	//printk("switched to! \n");
	output = 1;
	//enqueue_task_gwrr(rq, rq->curr, 1);
	//pick_next_task_gwrr(rq);
}

static const struct sched_class gwrr_sched_class = {
	.next			= &idle_sched_class,
	.enqueue_task		= enqueue_task_gwrr,
	.dequeue_task		= dequeue_task_gwrr,
	.yield_task		= yield_task_gwrr,


	.check_preempt_curr	= check_preempt_curr_gwrr,

	.pick_next_task		= pick_next_task_gwrr,
	.put_prev_task		= put_prev_task_gwrr,


	.set_curr_task          = set_curr_task_gwrr,
	.task_tick		= task_tick_gwrr,

	.prio_changed		= prio_changed_gwrr,
	.switched_to		= switched_to_gwrr,
};
