/*
 * Author: Phyo Thiha
 * CSC 456, Assignment 3
 * Description: Program to make a minimal function call
 * so that we can measure the cost of doing so.
 *
 * To Compile: 
 * $ make
 *
 * Usage:
 * $ ./function_call <integer_number_of_calls_over_which_to_take_the_average>
 *
 * If the 2nd argument is not provided, the program will default to 1000 calls.
 *
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include "hrtimer_x86.h"

#define DEFAULT_CALLS 1000

void minimum_function(){

}

int main(int argc, char** argv){
  
  int num_to_call = DEFAULT_CALLS;
  if(argc > 1)
    num_to_call = atoi(argv[1]);

  double total_time = 0;
  double start_time = 0;
  double end_time = 0;

  // these two variables below are for sanity check to make sure I'm not screwing up 
  // over C++'s numeric data types (that is, division of double point numbers
  //double total_time_1 = 0;
  //double total_time_2 = 0;

  for(int i = 0; i < num_to_call; i++){
    start_time = gethrtime_x86();
    minimum_function();
    end_time = gethrtime_x86();
    double diff = end_time - start_time;
    total_time += diff;
    //total_time_1 += diff / num_to_call;
    //total_time_2 += diff / (double) num_to_call;
  }

  double result = total_time / (double) num_to_call;

  printf("Avg. secs per minimum function call over %d iterations: %.9f\n", num_to_call, result);
  //printf("Avg. secs per minimum function call over %d iterations: %.9f\n", num_to_call, total_time_1);
  //printf("Avg. secs per minimum function call over %d iterations: %.9f\n", num_to_call, total_time_2);

  return 0;
}

