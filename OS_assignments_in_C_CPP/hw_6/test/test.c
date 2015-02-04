#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#define _MEMPAGESTATS 329

int main(int argc, char**argv) {
        int i;
	
	int memory;
	if(argc>0)
	memory = atoi(argv[1]);
	else
	memory = 5;
//        syscall(_MEMPAGESTATS);
	char *a;

        for(i = 0; i < 1; i++) {                

	        syscall(_MEMPAGESTATS);		
                a = (char*)malloc(memory*1024 * 1024 * sizeof(char));
                memset(a, '0', memory*1024* 1024* sizeof(char));
	        syscall(_MEMPAGESTATS);

                free(a);
        }

//        syscall(_MEMPAGESTATS);
//	free (a);

        return 0;
}
