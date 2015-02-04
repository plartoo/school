/*
 * Authors: Ian Perera and Phyo Thiha
 * CSC456/Assignment 4
 * 
 * Description: Implementation of mutex lock for kernel space.
 * 
 * Usage: 
 * // put the folder 'sync' in QDGL's linux folder
 * // place the main Makefile, syscall_table_32.S, and unistd.h to where they belong
 * // compile QEMU by 
 *      $ make
 * // boot QEMU, log in as 'cs2456' and place 'sync_test.c' under 'test_file' folder
 * // compile the test file in user mode and run the test
 *      $ gcc -o sync_test sync_test.c libsthread.a // must do this inside QEMU instance
 * 
 */

#include <linux/kernel.h>
#include <linux/slab.h>
#include <linux/list.h>
#include <linux/types.h>
#include <linux/wait.h>
#include <linux/sched.h>

struct Event
{
    int id;
    struct list_head list;
    wait_queue_head_t wq;
    bool condition;
    atomic_t numProcesses;
};

struct Event eventList;
static atomic_t nextId = ATOMIC_INIT(1);
DEFINE_MUTEX(eventListLock);

asmlinkage int sys_doevent_init()
{
    INIT_LIST_HEAD(&eventList.list);
    printk("doevent_init called.\n");
    return 0;
}

// Creates a new Event and adds it to the eventList.
asmlinkage int sys_doeventopen()
{
    struct Event * newEvent = (struct Event *)kmalloc(sizeof(struct Event),
                                                        GFP_KERNEL);
    
    if (newEvent == NULL)
        return -1;
    newEvent->condition = false;
    atomic_set(&(newEvent->numProcesses),0);
    newEvent->id = atomic_read(&nextId);

    // Initialize the waitqueue
    init_waitqueue_head(&(newEvent->wq));
    printk("doeventopen called id: %d\n", newEvent->id);
    
    // Add the event to the eventList
    mutex_lock(&eventListLock);
    list_add_tail(&(newEvent->list),&(eventList.list));
    mutex_unlock(&eventListLock);
    atomic_inc(&nextId);
    return newEvent->id;
}

// Assigns the calling process to wait for the signal of eventID
asmlinkage int sys_doeventwait(int eventID)
{
    struct list_head * pos;
    struct Event * temp;
    mutex_lock(&eventListLock);
    list_for_each(pos,&eventList.list)
    {

        temp = list_entry(pos, struct Event, list);
        if (temp->id == eventID)
        {
            printk("starting doeventwait for id: %d\n", eventID);
            atomic_inc(&(temp->numProcesses));
            mutex_unlock(&eventListLock);
            // Make the process wait until it gets the signal
            wait_event_interruptible(temp->wq,temp->condition);
            if (temp != NULL)
                temp->condition = false;
            printk("doeventwait complete for id:  %d\n", eventID);
            return 1;
        }
    }
    mutex_unlock(&eventListLock);
    printk("doeventwait: No such eventID %d\n", eventID);
    return -1;
}

// Signals the given eventID, releasing all associated proccess
asmlinkage int sys_doeventsig(int eventID)
{
    struct list_head * pos;
    struct Event * temp;
    mutex_lock(&eventListLock);
    list_for_each(pos,&eventList.list)
    {
        temp = list_entry(pos, struct Event, list);
        if (temp->id == eventID)
        {
            printk("Waking up eventID %d\n", eventID);
            temp->condition = true;
            wake_up_all(&(temp->wq)); 
            printk("Finished waking up eventID %d\n", eventID);
            mutex_unlock(&eventListLock);
            int numProcesses = atomic_read(&(temp->numProcesses));
            atomic_set(&(temp->numProcesses),0);
            return numProcesses;
        }
    }
    mutex_unlock(&eventListLock);
    printk("doeventsig: No such eventID %d\n", eventID);
    return -1;
}

// Releases all of the processes waiting on the event, and deletes it
asmlinkage int sys_doeventclose(int eventID)
{
    if (sys_doeventsig(eventID) >= 0)
    {
        struct list_head *pos, *q;
        struct Event * temp;
        mutex_lock(&eventListLock);
        list_for_each_safe(pos,q,&eventList.list)
        {
            temp = list_entry(pos, struct Event, list);
            if (temp->id == eventID)
            {
                int numProcesses = atomic_read(&(temp->numProcesses));
                printk("doeventclose id %d\n", eventID);
                // Delete the event from the linked list
                list_del(pos);
                // Free the memory associated with the event
                kfree(temp);
                mutex_unlock(&eventListLock);
                return numProcesses;
            }
        }
        mutex_unlock(&eventListLock);
    }
    printk("doeventclose: No such eventID %d\n", eventID);
    return -1;
}

