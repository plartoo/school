/*
 * Authors: Ian Perera and Phyo Thiha
 * CSC456/Assignment 4
 * 
 * Description: Implementation of mutex lock for user space.
 * 
 * Usage (along with the test file, named 'test.c'): 
 *      $ make clean // to clean the existing object and linker files
 *      $ make libsthread.a // compile the linker from fresh
 *      $ gcc -o test test.c libsthread.a // compile the test file
 * 
 */

#define _REENTRANT

#include "sthread.h"
#include <stdio.h>

int mutex_id = 0;

int sthread_mutex_init(sthread_mutex_t *mutex) {

    if(mutex->initialized == 1) // if already set, return -1
        return -1;

    mutex->initialized = 1;
    mutex->mutex_id = mutex_id; // assign id to mutex
    mutex_id++;

    mutex->acquired = 0;
    mutex->wait_queue_last_index = 0;
    mutex->request_count = 0;

    return 0;
}

int sthread_mutex_destroy(sthread_mutex_t *mutex) {
    mutex->initialized = 0;
    return 0;
}

void add_to_wait_queue(sthread_mutex_t * mutex, sthread_t thread) {
    mutex->waiting_list[(mutex->wait_queue_last_index)] = thread;
    mutex->wait_queue_last_index++;
}

// should obtain the lock, if it is available, 
// or else the current thread should block until the lock is available
int sthread_mutex_lock(sthread_mutex_t *mutex) {

    if((mutex == NULL) || (mutex->initialized != 1))
            return -1;

    int first_try = 0;

    // if this is the mutex lock is already acquired
    while(test_and_set(&(mutex->acquired)) != 0) {
        // if owner tries to get the lock (recursive case), just require 
        // another call to sthread_mutex_unlock() to fully unlock the mutex
        if(mutex->owner == sthread_self()) {
                //printf("Owner attempting to lock again: mutex_id = %d, owner_thread_id = %d \n", mutex->mutex_id, sthread_self());
                mutex->request_count++;
                return 0;
        }

        // if NOT the owner AND is the first try, put it on wait queue
        if(first_try == 0) {
                first_try = 1;
                add_to_wait_queue(mutex, sthread_self());
        }

        sthread_suspend(); // keeps waiting
    }

    // if not acquired, mark thread's pid as owner
    mutex->owner = sthread_self();
    mutex->request_count++;
    //printf("Lock acquired: mutex_id = %d, owner_thread_id = %d \n", mutex->mutex_id, sthread_self());

    // remove self from the waiting list after getting the lock
    int i;
    for(i = 0; i < mutex->wait_queue_last_index; i++) {
        if(mutex->waiting_list[i] == sthread_self())
            mutex->waiting_list[i] = NULL;
    }

    return 0;
}

// obtain the lock and return 0, if the lock is available, 
// or else return non-zero immediately. This function does NOT cause the caller to block
int sthread_mutex_trylock(sthread_mutex_t *mutex) {

    if((mutex == NULL) || (mutex->initialized != 1))
        return -1;

    if(test_and_set(&(mutex->acquired)) != 0) { // lock already acquired
        if(mutex->owner == sthread_self()) { // handle it for the owner (recursive case)
            //printf("Owner attempting to lock again: mutex_id = %d, owner_thread_id = %d \n", mutex->mutex_id, sthread_self());
            mutex->request_count++;
            return 0;
        }

        return 1; // return non-zero immediately
    }
    else { // lock is available
        mutex->owner = sthread_self();
        mutex->request_count++;

        return 0;
    }

}

// should make the lock available
// should be an error for any thread other than the owner of the lock to call this
int sthread_mutex_unlock(sthread_mutex_t *mutex) {

    if((mutex == NULL) || (mutex->initialized != 1) || (mutex->acquired == 0))
            return -1;

    // error if this thread is not an owner
    if(mutex->owner != sthread_self())
            return -1;

    mutex->request_count--;

    if(mutex->request_count == 0) { // wake all the waiting threads
        //printf("Mutex unLocked: mutex_id = %d, owner_thread_id = %d \n", mutex->mutex_id, sthread_self());
        int i;
        for(i = 0; i < mutex->wait_queue_last_index; i++) {
            if(mutex->waiting_list[i] == NULL)
                    continue;
            sthread_wake(mutex->waiting_list[i]);
            //printf("awake %d \n", mutex->waiting_list[i]);
        }

        mutex->acquired = 0;
    }

    return 0;
}


