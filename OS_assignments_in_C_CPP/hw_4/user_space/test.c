/*
 * Authors: Ian Perera and Phyo Thiha
 * CSC456/Assignment 4
 * 
 * Description: Test file of our implementation of mutex lock for user space.
 * 
 * Note: This test file is inspired by 'sample.c', which is found in the assignment folder.
 * 
 * Usage: 
 *      $ make clean // to clean the existing object and linker files
 *      $ make libsthread.a // compile the linker from fresh
 *      $ gcc -o test test.c libsthread.a // compile the test file
 * 
 */


#define _REENTRANT
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include "sthread.h"

sthread_mutex_t mutex1;
sthread_mutex_t mutex2;

int completed = 0;

// threads using this try to get both mutexes
int greedy_action(void *arg)
{
  int threadno = (int)arg;

  sthread_mutex_lock(&mutex1);
  sthread_mutex_lock(&mutex1); // to prove the recursive case

  sthread_mutex_lock(&mutex2);
  sleep(1);

  sthread_mutex_unlock(&mutex1);
  sthread_mutex_unlock(&mutex1); // release mutex1 entirely
  sthread_mutex_unlock(&mutex2);

  printf("Thread %d done\n", threadno);

  completed++;
  return 0;
}

// threads using this basically do nothing
int simple_action(void *arg)
{

  int threadno = (int)arg;
  printf("Simple thread %d: done\n", threadno);
  completed++;

  return 0;
}

int main(int argc, char *argv[])
{
  sthread_t t1, t2, t3, t4;

  if (sthread_init() == -1)
    fprintf(stderr, "%s: Error in sthread_init: %s\n", argv[0], strerror(errno));

  sthread_mutex_init(&mutex1);
  sthread_mutex_init(&mutex2);

  if (sthread_create(&t1, greedy_action, (void *)1) == -1)
    fprintf(stderr, "%s: sthread_create: %s\n", argv[0], strerror(errno));

  if (sthread_create(&t2, simple_action, (void *)2) == -1)
    fprintf(stderr, "%s: sthread_create: %s\n", argv[0], strerror(errno));
    
  if (sthread_create(&t3, greedy_action, (void *)3) == -1)
    fprintf(stderr, "%s: sthread_create: %s\n", argv[0], strerror(errno));
    
  if (sthread_create(&t4, greedy_action, (void *)4) == -1)
    fprintf(stderr, "%s: sthread_create: %s\n", argv[0], strerror(errno));
    
  while(completed != 4); // if there's deadlock, we'll get stuck here

  sthread_mutex_destroy(&mutex1);
  sthread_mutex_destroy(&mutex2);

  printf("All threads exited.\n");

  return 0;
}
