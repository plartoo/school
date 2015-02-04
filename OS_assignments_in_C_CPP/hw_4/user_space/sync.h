/*
 * Authors: Ian Perera and Phyo Thiha
 * CSC456/Assignment 4
 * 
 * Description: Header file for implementation of mutex lock for user space.
 * 
 */

#ifndef _STHREAD_SYNC_H_
#define _STHREAD_SYNC_H_

#define MAX_QUEUE_LENGTH 100

struct sthread_mutex_struct {
  int initialized;
  int mutex_id;

  unsigned long acquired;
  int wait_queue_last_index;
  int request_count;

  sthread_t owner;
  sthread_t waiting_list[MAX_QUEUE_LENGTH];
};

typedef struct sthread_mutex_struct sthread_mutex_t;

int sthread_mutex_init(sthread_mutex_t *mutex);
int sthread_mutex_destroy(sthread_mutex_t *mutex);
int sthread_mutex_lock(sthread_mutex_t *mutex);
int sthread_mutex_trylock(sthread_mutex_t *mutex);
int sthread_mutex_unlock(sthread_mutex_t *mutex);

#endif
