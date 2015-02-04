/*
 * Author: Phyo Thiha
 * CSC 456, Assignment 3
 * Description: Program that solves Traveling Salesman Problem
 * using brute force bran-and-bound approach.  
 * This program utilizes user-level (busy-waiting) synchronization, 
 * provided by class instructor for concurrency.
 *
 * To Compile: 
 * $ make
 *
 * Usage:
 * $ ./busywait_tsp <input_graph_of_trajectory> <num_of_threads_to_be_created>
 *
 * Note: <input_graph_of_trajectory> must contain the integer N, followed by 
 * (N2-N)/2 integers that constitute the upper triangle of the adjacency matrix of the graph. 
 * The program accepts arbitrary white space separation between integers. 
 * To see example of input files, please check "test1" and "test2" provided in the same folder
 * with this program file.
 *
 *
 */

#include <stdio.h>
#include <deque>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>
#include "atomic_ops.h"
#include "hrtimer_x86.h"

using namespace std;

struct path{
  deque<int> nodes;
  int current_cost;
  int num_nodes;
  deque<int> nodes_remaining;
};

int nnum_nodes;
int cur_best_cost = -1;
path cur_best_path;
deque<path> taskqueue;

int graph2[100][100];
pthread_mutex_t mutexbest;
pthread_mutex_t mutextask;
pthread_cond_t threads_cv;

int threads;
int waiting_threads = 0;
int finished = 0;
tatas_lock_t tasklock;
hrtime_t total_wait = 0;
int waited_threads = 0;

int cost(int i1, int i2){
  int lower = i1;
  int higher = i2;
  if(i2 < i1){
    lower = i2;
    higher = i1;
  }
  return graph2[lower][higher];
}

void print_path(path p){
  for(int j = 0; j < p.nodes.size(); j++){
    printf("%d\t", p.nodes.at(j) );
  }
  printf("\n\n");
}


int check_next_node(){
  tatas_acquire(&tasklock);
  waited_threads++;
  hrtime_t start_wait = gethrcycle_x86();
  if(taskqueue.empty()){
    waiting_threads++;
    if(waiting_threads == threads){
      finished = 1;
    }
    while(finished != 1 && taskqueue.empty()){
      tatas_release(&tasklock);
      tatas_acquire(&tasklock);
    }
  
    if(finished == 1){
      tatas_release(&tasklock);
      return -1;
    } else {
      waiting_threads--;
    }
  }

  path p = taskqueue.front();
  taskqueue.pop_front();
  tatas_release(&tasklock);
  hrtime_t end_wait = gethrcycle_x86();
  hrtime_t diff_wait = end_wait - start_wait;
  total_wait += diff_wait;
 
  // calculate next cost
  int remaining_paths = p.nodes_remaining.size();
  path remaining_array[remaining_paths];
  int cur_num = p.num_nodes + 1;
  int current_cost = p.current_cost;
  int last_in_path = p.nodes.back();
  for(int i = 0; i < remaining_paths; i++){
    int node_next = p.nodes_remaining.at(i);
    p.nodes_remaining.erase(p.nodes_remaining.begin()+i);

    remaining_array[i].nodes_remaining = deque<int>(p.nodes_remaining);
    remaining_array[i].nodes = deque<int>(p.nodes);
    remaining_array[i].current_cost = current_cost + cost(last_in_path, node_next);
    remaining_array[i].nodes.push_back(node_next);
    remaining_array[i].num_nodes = cur_num;

    p.nodes_remaining.insert(p.nodes_remaining.begin()+i, node_next);
  }

  // add all to queue if right #
  if(cur_num < nnum_nodes){
    tatas_acquire(&tasklock);
    for(int j = 0; j < remaining_paths; j++){
      if(cur_best_cost == -1 || remaining_array[j].current_cost <= cur_best_cost){
        taskqueue.push_front(remaining_array[j]);
      }
    }
    tatas_release(&tasklock);
  } else { 
    tatas_acquire(&tasklock);
    for(int j = 0; j < remaining_paths; j++){
      if(cur_best_cost == -1 || remaining_array[j].current_cost < cur_best_cost){
        cur_best_cost = remaining_array[j].current_cost;
        cur_best_path = remaining_array[j];
      }
    }
    tatas_release(&tasklock);
  }

  return 0;
}


void *threaded_check_node(void *arg){
  int i = 0;
  while(i != -1)
    i = check_next_node();
  pthread_exit((void*) 0);
}

int main(int argc, char ** argv){
  if(argc < 3){
    printf("Please provide the name of an input file, and the number of threads to run.\n");
    exit(1);
  }
  
  FILE * input_file;
  input_file = fopen(argv[1], "r");
  fscanf(input_file, "%d", &nnum_nodes);
 
  path p;
  p.nodes = deque<int>(1,0);

  for(int m = 0; m < nnum_nodes-1; m++){
    for(int n = m+1; n < nnum_nodes; n++){
      int num;
      fscanf(input_file, "%d", &num);
      graph2[m][n] = num;
    }
    p.nodes_remaining.push_front(m+1);    
  }
  
  fclose(input_file);
  p.num_nodes = 1;
  p.current_cost = 0;
  taskqueue.push_front(p);
  int i = 0;

  threads = atoi(argv[2]);
  pthread_setconcurrency(threads);
  pthread_t thread_array[threads];

  pthread_mutex_init(&mutexbest, NULL);
  pthread_mutex_init(&mutextask, NULL);

  pthread_cond_init(&threads_cv, NULL);
 
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
  pthread_attr_setscope(&attr, PTHREAD_SCOPE_SYSTEM);

  hrtime_t starttime;
  hrtime_t starttime2;
  hrtime_t endtime;

  for(int k = 0; k < threads; k++){
    pthread_create(&thread_array[k], &attr, threaded_check_node, (void *)k);
    if(k == 0)
      starttime = gethrcycle_x86();
  }
      starttime2 = gethrcycle_x86();

  pthread_attr_destroy(&attr);

  for(int k = 0; k < threads; k++){
    void * status;
    pthread_join(thread_array[k], &status);
    if(k == 0)
      endtime = gethrcycle_x86();
  }

  hrtime_t diff = endtime-starttime;
  hrtime_t time = diff / getMHZ_x86();
  printf("Total time (in microsecond): \t\t%lld\n", time);

  total_wait = total_wait / getMHZ_x86();
  printf("Total wait-time (in microsecond): \t%lld\n", total_wait);
  total_wait = total_wait / waited_threads;
  printf("Average wait-time (in microsecond): \t%lld\n", total_wait);

  pthread_mutex_destroy(&mutexbest);
  pthread_mutex_destroy(&mutextask);
  pthread_cond_destroy(&threads_cv);

  printf("Best Path (cost: %d): \t", cur_best_cost); 
  print_path(cur_best_path);

  pthread_exit(NULL);

  return 0;
}


