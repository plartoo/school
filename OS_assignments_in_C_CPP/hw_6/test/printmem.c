#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define _MEMPAGESTATS 329

int main() {
        int i;
        printf("---------------\n");

        syscall(_MEMPAGESTATS);
	printf("---------------\n");
	return 0;
}
