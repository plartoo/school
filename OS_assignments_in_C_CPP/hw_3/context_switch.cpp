/*
 * Author: Phyo Thiha
 * CSC 456, Assignment 3
 * Description: Program to measure the cost of a process context switch.
 *
 * To Compile: 
 * $ make
 *
 * Usage:
 * $ ./context_switch <integer_number_of_calls_over_which_to_take_the_average>
 *
 * If the 2nd argument is not provided, the program will default to 1000 calls.
 *
 *
 */


#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <wait.h>
#include "hrtimer_x86.h"

#define DEFAULT_CALLS 1000

int main(int argc, char ** argv){
  int to_child[2];
  int to_parent[2];
  char buffer[1];
  buffer[0] = 'a';

  int num_to_call = DEFAULT_CALLS;
  if(argc > 1)
    num_to_call = atoi(argv[1]);

  if( pipe(to_child) < 0 || pipe(to_parent) < 0){
    printf("Problem creating pipes.\n");
    exit(1);
  }

  pid_t pid;
  double total_time = 0;
 
  if((pid = fork()) == 0){ 
    close(to_child[1]); // close parent's write end that is inherited
    close(to_parent[0]); // close child's read end that is not going to be used
    for(int i = 0; i < num_to_call; i++){
      int bytes_read = read(to_child[0], buffer, 1); // read from where parent is writing
      int bytes_written = write(to_parent[1], buffer, 1); // write to designated channel
    }
    close(to_child[0]);
    close(to_parent[1]);
  } 
  else {
    close(to_child[0]);
    close(to_parent[1]);
    for(int i = 0; i < num_to_call; i++){
      double start_time = gethrtime_x86();
      int bytes_written = write(to_child[1], buffer, 1);
      int bytes_read = read(to_parent[0], buffer, 1);
      double end_time = gethrtime_x86();
      total_time += (end_time - start_time);
    }
    close(to_child[1]);
    close(to_parent[0]);
    waitpid(-1, NULL, 0);
 
    double result = total_time / (double) num_to_call;
    printf("Average secs per context switch: %.9f\n", result);
  }
  return 0;
}


