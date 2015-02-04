/* 
 * Author: Phyo Thiha
 * Date: September 02, 2011
 *
 * For: CSC 456, Assignment 1/Part I
 * Version 1: parses the
 * 	- Processor type(s)
 * 	- Kernel version
 * 	- The amt. of memory configured into the computer
 * 	- Amt. of time since the system was last booted
 *
 * Version 2: print lists of following values based on specific interval
 * 	- % of CPU time in user mode, system mode, and % of time CPU is idle
 * 	- the amt. and % of available/free memory
 * 	- rate of disk read/write (in # sectors/sec)
 * 	- rate of context switches in the kernel (# per sec)
 * 	- rate of process creations in the system (# per sec)
 *
 * To compile:
 * $ make 
 *
 * To run:
 * (Version 1): $ ./proc_parse
 * (Version 2): $ ./proc_parse <read_rate> <printout_rate>, where
 * 		<read_rate> and <printout_rate> MUST be integers
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define STR_BUFFER 1000
#define STR_LEN_TO_COMPARE 10

int get_proc(char* proc_file, char* str_to_match, char* header)
{

	FILE *ptr_file;
	char line[STR_BUFFER];

	ptr_file = fopen(proc_file, "r");
	if (!ptr_file)
		return 1;

	printf("\n%s\n==============\n", header);

	while (fgets(line, STR_BUFFER, ptr_file) != NULL){
		if (strlen(str_to_match) == strspn(line, str_to_match))
			printf("%s", line);
	}
	fclose(ptr_file);

	return 0;
}

void print_break(){
	printf("\n=======================\n");
}

float get_percentage(float numerator, float denominator){
	float f;
	f = (numerator * 100.0)/denominator;
	return f;
}

float get_token(FILE* proc_file, char* str_to_match, int token_to_print){

	char temp[STR_BUFFER];
	char delims[] = " ";
	char *result = NULL;
	int counter = 0;
	float value = -1;

	while (fgets(temp, STR_BUFFER, proc_file) != NULL){
		if ((strstr(temp, str_to_match)) != NULL){
			result = strtok(temp, delims);

			while (result != NULL){
				counter += 1;
				if (counter == token_to_print){
					value = atof(result);
				}
				result = strtok(NULL, delims);
			}
		}
	}

	return value;
}

int get_stats(int read_rate, int print_rate)
{

	FILE *ptr_file;
	float user, nice, system, idle;
	float cur_user, cur_nice, cur_system, cur_idle, cur_total;

	float mem_total, mem_free;

	float initial_write_value, initial_read_value, cur_read_value, cur_write_value;

	float initial_ctxt_value, cur_ctxt_value;
	
	float initial_process_num, cur_process_num;

	int i, iteration;

	iteration = print_rate/read_rate;

	while (1){
		cur_user = 0;
		cur_nice = 0;
		cur_system = 0;
		cur_idle = 0;
		cur_total = 0;
		
		initial_read_value = 0;
		initial_write_value = 0;
		cur_read_value = 0;
		cur_write_value = 0;

		initial_ctxt_value = 0;
		cur_ctxt_value = 0;

		initial_process_num = 0;
		cur_process_num = 0;

		print_break();

		for (i=0; i < iteration; i++){
			ptr_file = fopen("/proc/stat", "r");
			if (!ptr_file)
				return 1;

			// For CPU time related information, I assumed the first four entries
			// will sum to the total CPU time as suggested by Professor Dwarkadas
			fscanf(ptr_file, "cpu  %f %f %f %f", &user, &nice, &system, &idle);

			// keep the average because the cumulative aggregate can cause number overflow
			cur_user = (cur_user + user)/2.0;
			cur_nice = (cur_nice + nice)/2.0;

			cur_system = (cur_system + system)/2.0;
			cur_idle = (cur_idle + idle)/2.0;
			cur_total = (cur_total + (user + nice + system + idle))/2.0;

			// For available free memory
			ptr_file = fopen("/proc/meminfo", "r");
			if (!ptr_file)
				return 1;
			mem_free = get_token(ptr_file, "MemFree", 2);

			ptr_file = fopen("/proc/meminfo", "r");
			if (!ptr_file)
				return 1;
			mem_total = get_token(ptr_file, "MemTotal", 2);


			// For sector read/write values:
			// according to iostats.txt <http://www.mjmwired.net/kernel/Documentation/iostats.txt>,
			// from Kernel version 2.6+, 'proc/diskstats' returns 11 fields for each disk device.
			// Of those 11 fields, #3 and #7 represents num. of sectors read/written respectively.
			ptr_file = fopen("/proc/diskstats", "r");
			if (!ptr_file)
				return 1;

			if (initial_read_value == 0)
				initial_read_value = get_token(ptr_file, "sda ", 6);
			else
				cur_read_value = get_token(ptr_file, "sda ", 6);


			ptr_file = fopen("/proc/diskstats", "r");
			if (!ptr_file)
				return 1;

			if (initial_write_value == 0)
				initial_write_value = get_token(ptr_file, "sda ", 10);
			else
				cur_write_value = get_token(ptr_file, "sda ", 10);

			// For context switch rate
			ptr_file = fopen("/proc/stat", "r");
			if (!ptr_file)
				return 1;
			
			if (initial_ctxt_value == 0)
				initial_ctxt_value = get_token(ptr_file, "ctxt", 2);
			else
				cur_ctxt_value = get_token(ptr_file, "ctxt", 2);


			// For num. of process created since boot
			ptr_file = fopen("/proc/stat", "r");
			if (!ptr_file)
				return 1;
			
			if (initial_process_num == 0)
				initial_process_num = get_token(ptr_file, "processes", 2);
			else
				cur_process_num = get_token(ptr_file, "processes", 2);

			fflush(stdout);
			fclose(ptr_file);
			sleep(read_rate);
		}

		printf("Percentage of (collective) CPU time-\n");
		printf("user mode: %.2f\n", get_percentage(cur_user, cur_total));
		printf("system mode: %.2f\n", get_percentage(cur_system, cur_total));
		printf("idle: %.2f\n\n", get_percentage(cur_idle, cur_total));

		printf("Amount and percentage of available (or free) memory in kB\n");
		printf("amount: %.0f\n", mem_free);
		printf("percentage free: %.2f\n\n", get_percentage(mem_free,mem_total));

		printf("Rate (number per second) of disk read/write in the system:\n");
		printf("sectors read: %.0f\n", ((cur_read_value - initial_read_value)/print_rate));
		printf("sectors written: %.0f\n\n", ((cur_write_value - initial_write_value)/print_rate));

		printf("Rate (number per second) of context swtiches:\n");
		printf("%.0f\n\n", ((cur_ctxt_value - initial_ctxt_value)/print_rate));

		printf("Rate (number per second) of processes:\n");
		printf("%.0f\n", ((cur_process_num - initial_process_num)/print_rate));
		
		fflush(stdout); // per Li Lu's suggestion
		print_break();
	}
	
	return 0;
}

int main(int argc, char *argv[])
{
	int read_rate;
	int print_rate;

	if (argc == 1){
		get_proc("/proc/cpuinfo", "model name", "CPU Type(s): ");
		get_proc("/proc/version", "", "Kernel Version: ");
		get_proc("/proc/meminfo", "MemTotal", "Total amt. of memory (in KB): ");
		get_proc("/proc/uptime", "", "Amt. of time since last boot (in seconds): ");
	}
	else if (argc == 3){
		read_rate = atoi(argv[1]);
		print_rate = atoi(argv[2]);
		get_stats(read_rate, print_rate);
	}
	return 0;
}

