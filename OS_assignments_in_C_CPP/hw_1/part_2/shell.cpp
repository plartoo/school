/*
 * CSC456: Assignment 1/ PartII
 * Phyo Thiha
 * September 08, 2011
 * Implementation of a simple shell.
 *
 * To compile: type 'make' in the folder where the source file exists.
 * To run: type './shell' at the command prompt
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <iostream>
#include <vector>
#include <cstring>
#include <cerrno>
#include <fstream>
#include <strings.h>

// for fork, waitpid
#include <sys/types.h>
#include <sys/wait.h>

// for signal handlers
#include <signal.h>
//#include <setjmp.h>

#define PROMPT "MarioShell$ "
#define DEBUG 1 // '1' to print debug statements

// CONSTANT values for 'exit', 'cd' and 'jobs'
#define EXIT "exit"
#define CHDIR "cd"
#define PWD "pwd"
#define JOBS "jobs"

#define DELIMITERS " \t"
#define STR_BUFFER 10000

// CONSTANT values for 'fork', waitpid and 'execv'
#define BACKGROUND_TASK_CHAR "&"


// copied from "sh-skeleton.c"
/* Misc manifest constants */
#define MAXLINE    1024   /* max line size */
#define MAXARGS     128   /* max args on a command line */
#define MAXJOBS      16   /* max jobs at any point in time */
#define MAXJID    1<<16   /* max job ID */

/* Job states */
#define UNDEF 0 /* undefined */
#define FG 1    /* running in foreground */
#define BG 2    /* running in background */
#define ST 3    /* stopped */

#define FOREGROUND "fg"
#define BACKGROUND "bg"

/* 
 * Jobs states: FG (foreground), BG (background), ST (stopped)
 * Job state transitions and enabling actions:
 *     FG -> ST  : ctrl-z
 *     ST -> FG  : fg command
 *     ST -> BG  : bg command
 *     BG -> FG  : fg command
 * At most 1 job can be in the FG state.
 */

/* Global variables */
extern char **environ;      /* defined in libc */
int verbose = 0;            /* if true, print additional output */
int nextjid = 1;            /* next job ID to allocate */
char sbuf[MAXLINE];         /* for composing sprintf messages */

struct job_t {              /* The job struct */
    pid_t pid;              /* job PID */
    int jid;                /* job ID [1, 2, ...] */
    int state;              /* UNDEF, BG, FG, or ST */
    char cmdline[MAXLINE];  /* command line */
};
struct job_t jobs[MAXJOBS]; /* The job list */
/* End global variables */


/* Function prototypes from "sh-skeleton.c"*/
void sigchld_handler(int sig);
void sigtstp_handler(int sig);
void sigint_handler(int sig);

/* Here are helper routines that we've provided for you */
void sigquit_handler(int sig);

void clearjob(struct job_t *job);
void initjobs(struct job_t *jobs);
int maxjid(struct job_t *jobs); 
int addjob(struct job_t *jobs, pid_t pid, int state, char *cmdline);
int deletejob(struct job_t *jobs, pid_t pid); 
pid_t fgpid(struct job_t *jobs);
struct job_t *getjobpid(struct job_t *jobs, pid_t pid);
struct job_t *getjobjid(struct job_t *jobs, int jid); 
int pid2jid(pid_t pid); 
void listjobs(struct job_t *jobs);

// prototype for Phyo's functions
int wait_for_fg(pid_t pid);


using namespace std;

// for parsing original command and checking '&' in it
// (assumes '&' is provided separated by a "space"/"tab" character from the last argument)
bool parse_command(string orig_cmd, vector<string>* parsed_cmds){
	bool bg = false;
	string bgchar = BACKGROUND_TASK_CHAR;
	char* util = 0;
	char* sptr;

	// strtok_r is a threadsafe version of strtok
	util = strtok_r(&orig_cmd[0], DELIMITERS, &sptr);

	if (util == NULL){// this means there's no token as a result of parsing (in other words, the command entered is empty
		parsed_cmds->push_back("\0");
	}
	else{
		do
		{

			if (bgchar.compare(util) == 0)
				bg = true;
			else
				parsed_cmds->push_back(util);

		}while (util = strtok_r(NULL, DELIMITERS, &sptr));
	}

	return bg;
}

// prepare argument array for "execvp" - push arguments from
// 'cmds' into args_holder and push NULL as the last element
void prepare_arguments_for_execvp(vector<string>* cmds, char* args_holder[]){
	int i;
	int cmds_arg_len = cmds->size() - 1;

	if (cmds_arg_len > MAXARGS){
		cout << "Your command's argument length is more than allowed "<< MAXARGS << endl;
		exit(0);
	}

	for (i=0; i < cmds->size(); i++){
		args_holder[i] = &cmds->at(i)[0];
	}
	args_holder[i] = NULL;
}

// for "exit"
void handle_exit(int cmd_size){
	if (cmd_size == 1)
		exit(EXIT_SUCCESS); // same as "exit(0)"
	else
		cout << "exit: Expression Syntax." << endl;
}

// print current working directory
void print_pwd(){
        char pwd[MAXLINE] = ".";

        if ( NULL == getcwd(pwd, MAXLINE) )
        {
                perror("print_pwd Error");
        }
        cout << pwd << endl;
}

// for "cd"
int change_dir(char* new_dir){

	if ( -1 == chdir(new_dir)){
		perror("chdir Error");
		return 0;
	}
	return 1;
}

void handle_chdir(vector<string> cmds){

	char current_directory[MAXLINE] = ".";

	if (cmds.size() > 2)
		cout << "cd: Too many arguments." << endl;
	// if command is "cd", just go to the current directory -> "."
	else if (cmds.size() == 1)
		change_dir(current_directory);
	else
		change_dir(&cmds.at(1)[0]);	

}

// do nothing for empty string commands
void do_nothing(){
	return;
}

// to test fork
void *do_work(const char *where)
{
	int i;
	int b=1,a=1;
	cout << "Entered work function " << where << endl;
	fflush(stdout);
	for (i=0;i<100000000;i++)
		b=a+b;
	cout << "Leaving work function " << where << endl;
	return (void *)b;
}

/*
// handles SIGCHLD (old approach without considering jobs table)
void sigchld_handler(int sig){
	pid_t pid;
	// wait until all child processes
	while ((pid = waitpid(-1, NULL, 0)) > 0){
		// delete from job table etc
		cout << "Parent reaped child: " << (int)pid << endl;
	}
	if (errno != ECHILD)
		cout << "waitpid error in sigchld handler" << endl;
	return;
}
*/

/*
 *  sigquit_handler - The driver program can gracefully terminate the
 *  child shell by sending it a SIGQUIT signal.
 *  (directly refernced from "sh-skeleton.c")
 *    
*/
void sigquit_handler(int sig) 
{
    cout << "Terminating after receipt of SIGQUIT signal" << endl;
    exit(1);
}

// install signal handlers
void install_sighandlers(){
	if (signal(SIGINT, sigint_handler) == SIG_ERR) // ctrl-c
		cout << "sigint error" << endl;
	if (signal(SIGTSTP, sigtstp_handler) == SIG_ERR) // ctrl-z
		cout << "sigstp error" << endl;
	if (signal(SIGCHLD, sigchld_handler) == SIG_ERR) // sent to terminated or stopped
		cout << "sigchld error" << endl;
	if (signal(SIGQUIT, sigquit_handler) == SIG_ERR) // killed
		cout << "sigquit error" << endl;
	return;
}

int main ()
{
	string orig_cmd;
	pid_t pid;

	install_sighandlers();

	/* Redirect stderr to stdout (so that driver will get all output
	* on the pipe connected to stdout) */
	dup2(1, 2);

	/* Initialize the job list */
	initjobs(jobs);

	while (1){
		vector<string> commands;
		bool background_task;

		cout << PROMPT ;
		getline(cin, orig_cmd);
		background_task = parse_command(orig_cmd, &commands);

/*
 		// check to see if parsing works correctly
		for (int i=0; i < commands.size(); ++i){
			cout << "cmd " << i <<":\t" << commands.at(i) << endl;
		}
*/

		if (commands.at(0).compare(EXIT) == 0)
			handle_exit(commands.size());
		else if (commands.at(0).compare(CHDIR) == 0)
			handle_chdir(commands);
		else if (commands.at(0).compare(PWD) == 0)
			print_pwd();
		else if (commands.at(0).compare(JOBS) == 0)
			listjobs(jobs);
		else if (commands.at(0).compare(FOREGROUND) == 0){
			// Sorry, didn't have enough time left to refactor this chunk out of main()!!
			if (commands.size() != 2)
				cout << "Usage: fg <job_id_as_specified_in_the_return_of_jobs_command>" << endl;
			else{
				int jid_for_fg = atoi(&commands.at(1)[0]);
//				if (DEBUG)
					cout << "FG: bring job id: " << jid_for_fg << " to foreground" << endl;

				struct job_t * job_fg = getjobjid(jobs, jid_for_fg);
				if (job_fg == NULL)
					cout << "Error: can't find job with jid: " << jid_for_fg << endl;
				else if (job_fg->state == ST){// stopped job should continue
					cout << "sending kill SIGCONT to pid: " << job_fg->pid << endl;
					if (kill(job_fg->pid, SIGCONT) != 0)
						cout << "Error: " << strerror(errno) << endl;
					else{
						cout << "FG should be set to state 3: " << job_fg->state << endl;
						job_fg->state = FG;
					}
				}
				else if (job_fg->state == BG){// already running background job should be set to FG
					job_fg->state = FG;
				}
				else{
					cout << "This process is not suspended." << endl;
				}

				// after setting the states and sending appropriate signal, it's time to put this in the foreground and wait for it to finish
				if (wait_for_fg(job_fg->pid) < 0)
					cout << "Error: wiaking up (waiting for) fg pid: " << job_fg->cmdline << endl;

				fflush(stdout);
				cout << "Did wake up fg pid: " << job_fg->pid << endl;
				fflush(stdout);
			}
		}

		else if (commands.at(0).compare(BACKGROUND) == 0){
			// Sorry, didn't have enough time left to refactor this chunk out of main()!!
			if (commands.size() != 2)
				cout << "Usage: bg <job_id_as_specified_in_the_return_of_jobs_command>" << endl;
			else{
				int jid_for_bg = atoi(&commands.at(1)[0]);
//				if (DEBUG)
					cout << "BG: put job id: " << jid_for_bg << " to background" << endl;

				struct job_t * job_bg = getjobjid(jobs, jid_for_bg);
				if (job_bg == NULL)
					cout << "Error: can't find job with jid: " << jid_for_bg << endl;
				else if (job_bg->state == ST){// only stopped job should be able to brought back with "bg" command
				cout << "sent kill SIGCONT to pid: " << job_bg->pid << endl;
					if (kill(job_bg->pid, SIGCONT) != 0)
						cout << "Error: " << strerror(errno) << endl;
					else{
						cout << "set the job to BG state: " << job_bg->state << endl;
						job_bg->state = BG;
					}
				}
				else{
					cout << "This process is not suspended." << endl;
				}
			}
		}

		else if (commands.at(0) == "\0")
			do_nothing(); // empty line as command, do nothing
		else if (commands.size() >= 1){
			// add '2' for NULL terminating char and the command line 
			char* argv[MAXLINE+2];
			prepare_arguments_for_execvp(&commands, argv);
/*
			// for pipe
			int status, pipefds[2];

			if ((status = pipe(pipefds)) < 0){
				perror("opening pipe problem");
				exit(EXIT_FAILURE);
			}
*/

			pid = fork();

			if (pid < 0){
				cerr << "failed to fork " << errno << endl;
				exit(EXIT_FAILURE);
			}
			else if (pid == 0){ // child process

/*
				// close write pipe because child only needs to receive data from parent
				close(pipefds[1]);
*/

/*
				// use below to test error throwinng when arg limit over
				for (int i=0; i < commands.size(); i++){
					cout << "child args holder: " << string(argv[i]) << endl;
				}
*/
      				if(DEBUG)
					cout << "I'm the child\tpid: " << (int)getpid() << "\tpgid: "<< getpgrp() << endl;

				if (execvp(argv[0], argv) < 0){
					cout << "Command not found: " << (string)argv[0] << endl;
//					close(pipefds[0]); // close the remaining fd when error happens
					exit(0);
				}

//				close(pipefds[0]); // close the remaining fd
			}
			else // parent process
			{
/*
				// pipe stuff
				close(pipefds[0]); // parent doesn't need to read anything from child
*/

      				if(DEBUG){
					cout << "Child spawned => child pid: " << pid << "\tcmd: " << orig_cmd << endl;
					cout << "Parent's pid: " << (int)getpid() << "\tpgid: " << getpgrp() << endl;
				}

				if (background_task == true){
					if (addjob(jobs, pid, BG, argv[0]) <= 0){
						cout << "Error adding job for pid: " << pid << endl;
//						close(pipefds[1]);
						exit(0);
					}
					if (DEBUG)
						cout << "Parent: child is background. don't wait" << endl;
				}
				else{// not background task
					// first: register the pid in the jobs table
					if (addjob(jobs, pid, FG, argv[0]) <= 0){
						cout << "Error adding job for pid: " << pid << endl;
//						close(pipefds[1]);
						exit(0);
					}

					if (wait_for_fg(pid) < 0){
						cout << "Waiting for fg child pid failed: " << pid << endl;
//						close(pipefds[1]);
					}
      					if(DEBUG)
						cout << "Parent: child is fg.  waited, now minding my own business" << endl;
				}
/*
				// make sure we close parent's stdout before ending the process
				close(pipefds[1]);
*/
			}
		}
	}

  return 0;
}

/* function to handle foreground signal stuff */
// wait for this process as it's set to run from fg
// 0 if things go well, -1 if errored
int wait_for_fg(pid_t pid){
      int status; 

      // WUNTRACED also return for children who are currently stopped
      if (waitpid(pid, &status, WUNTRACED) < 0){
        cout << "wait_for_fg: waitpid error" << endl;
        return -1;
      }

	// returns true if the child that caused the return is currently stopped 
      if(WIFSTOPPED(status)){
	if (DEBUG)
		cout << "This pid is currently stopped: " << (int)pid << endl;
        struct job_t * jobStopped;
        jobStopped = getjobpid(jobs, pid);
        jobStopped->state = ST;
      }

      if(WIFEXITED(status)){ // child exited normally, so clean up job table
	if (DEBUG)
		cout << "exited normallly" << endl;
        if(deletejob(jobs, pid) == 0){
          cout << "job table deletion error" << endl;
          return -1;
        }
      }
    fflush(stdout);
  return 0;
}

/*****************
 * Signal handlers
 *****************/

/* 
 * sigchld_handler - The kernel sends a SIGCHLD to the shell whenever
 *     a child job terminates (becomes a zombie), or stops because it
 *     received a SIGSTOP or SIGTSTP signal. The handler reaps all
 *     available zombie children, but doesn't wait for any other
 *     currently running children to terminate.  
 */
void sigchld_handler(int sig) 
{
    int child_status;
    pid_t pid;
    // WNOHANG means return immediately if no child has exited
    while((pid = waitpid(-1, &child_status, WNOHANG)) > 0){
      if(DEBUG)
	cout << "HANDLING SIGNAL FOR pid: " << pid << "\tstatus: " << child_status << endl;

      if(deletejob(jobs, pid) == 0)
          cout << "job table deletion error" << endl;
    }
	fflush(stdout);
    return;
}

/* 
 * sigint_handler - The kernel sends a SIGINT to the shell whenver the
 *    user types ctrl-c at the keyboard.  Catch it and send it along
 *    to the foreground job.  
 */
void sigint_handler(int sig) 
{
    	pid_t fg_proc = fgpid(jobs);
	if (DEBUG)
		cout << "SIGINT invoked for fg_process with pid: " << (int)fg_proc << endl;

	if(fg_proc > 0){
		cout << "sending SIGINT to pid: " << (int)fg_proc << endl;
		kill(fg_proc, SIGINT);

      	if(deletejob(jobs, fg_proc) == 0)
		cout << "job table deletion error" << endl;
    	}
    	else {
		cout << "\nno fg process to kill" << endl; // will not kill the shell if ctrl-c is pressed
	}

	fflush(stdout);
	return;
}

/*
 * sigtstp_handler - The kernel sends a SIGTSTP to the shell whenever
 *     the user types ctrl-z at the keyboard. Catch it and suspend the
 *     foreground job by sending it a SIGTSTP.  
 */
void sigtstp_handler(int sig) 
{
    pid_t fg_pid = fgpid(jobs);
    if (DEBUG)
	cout << "running as fg is pid: " << (int)fg_pid << endl;

    if(fg_pid > 0){
	if (DEBUG)
		cout << "almost sending SIGSTOP" << endl;

	if( kill(fg_pid, SIGSTOP) != 0){
		cout << "suspending fg process failed with errno: " << strerror(errno) << endl;
	}
	else {
		if (DEBUG)
			cout << "after killing with SIGSTOP to fgpid: " << (int)fg_pid << "\tset its state to ST"<< endl;
		struct job_t * thisjob;
		thisjob = getjobpid(jobs, fg_pid);
		thisjob->state = ST;
	}
    }
    else {
	cout << "\nno fg process to suspend" << endl;
    }

    fflush(stdout);
    return;
}

/*********************
 * End signal handlers
 *********************/

// copied from "sh-skeleton.c"
/***********************************************
 * Helper routines that manipulate the job list
 **********************************************/

/* clearjob - Clear the entries in a job struct */
void clearjob(struct job_t *job) {
    job->pid = 0;
    job->jid = 0;
    job->state = UNDEF;
    job->cmdline[0] = '\0';
}

/* initjobs - Initialize the job list */
void initjobs(struct job_t *jobs) {
    int i;

    for (i = 0; i < MAXJOBS; i++)
	clearjob(&jobs[i]);
}

/* maxjid - Returns largest allocated job ID */
int maxjid(struct job_t *jobs) 
{
    int i, max=0;

    for (i = 0; i < MAXJOBS; i++)
	if (jobs[i].jid > max)
	    max = jobs[i].jid;
    return max;
}

/* addjob - Add a job to the job list */
int addjob(struct job_t *jobs, pid_t pid, int state, char *cmdline) 
{
    int i;
    
    if (pid < 1)
	return 0;

    for (i = 0; i < MAXJOBS; i++) {
	if (jobs[i].pid == 0) {
	    jobs[i].pid = pid;
	    jobs[i].state = state;
	    jobs[i].jid = nextjid++;
	    if (nextjid > MAXJOBS)
		nextjid = 1;
	    strcpy(jobs[i].cmdline, cmdline);
  	    if(verbose){
	        printf("Added job [%d] %d %s\n", jobs[i].jid, jobs[i].pid, jobs[i].cmdline);
            }
            return 1;
	}
    }
    printf("Tried to create too many jobs\n");
    return 0;
}

/* deletejob - Delete a job whose PID=pid from the job list */
int deletejob(struct job_t *jobs, pid_t pid) 
{
    int i;

    if (pid < 1)
	return 0;

    for (i = 0; i < MAXJOBS; i++) {
	if (jobs[i].pid == pid) {
	    clearjob(&jobs[i]);
	    nextjid = maxjid(jobs)+1;
	    return 1;
	}
    }
    return 0;
}

/* fgpid - Return PID of current foreground job, 0 if no such job */
pid_t fgpid(struct job_t *jobs) {
    int i;

    for (i = 0; i < MAXJOBS; i++)
	if (jobs[i].state == FG)
	    return jobs[i].pid;
    return 0;
}

/* getjobpid  - Find a job (by PID) on the job list */
struct job_t *getjobpid(struct job_t *jobs, pid_t pid) {
    int i;

    if (pid < 1)
	return NULL;
    for (i = 0; i < MAXJOBS; i++)
	if (jobs[i].pid == pid)
	    return &jobs[i];
    return NULL;
}

/* getjobjid  - Find a job (by JID) on the job list */
struct job_t *getjobjid(struct job_t *jobs, int jid) 
{
    int i;

    if (jid < 1)
	return NULL;
    for (i = 0; i < MAXJOBS; i++)
	if (jobs[i].jid == jid)
	    return &jobs[i];
    return NULL;
}

/* pid2jid - Map process ID to job ID */
int pid2jid(pid_t pid) 
{
    int i;

    if (pid < 1)
	return 0;
    for (i = 0; i < MAXJOBS; i++)
	if (jobs[i].pid == pid) {
            return jobs[i].jid;
        }
    return 0;
}

/* listjobs - Print the job list */
void listjobs(struct job_t *jobs) 
{
    int i;
    
    for (i = 0; i < MAXJOBS; i++) {
	//cout << "useless job list: " << i << endl;
	if (jobs[i].pid != 0) {
	    printf("[%d] (%d) ", jobs[i].jid, jobs[i].pid);
	    switch (jobs[i].state) {
		case BG: 
		    printf("Running ");
		    break;
		case FG: 
		    printf("Foreground ");
		    break;
		case ST: 
		    printf("Stopped ");
		    break;
	    default:
		    printf("listjobs: Internal error: job[%d].state=%d ", 
			   i, jobs[i].state);
	    }
	    printf("%s\n", jobs[i].cmdline);
	}
    }
}
/******************************
 * end job list helper routines
 ******************************/


