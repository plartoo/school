/*
 * My approach: created 9 free lists of fixed sizes for allocating blocks.
 * An explicit list is used for blocks larger than the fixed sizes. I 
 * only coalese and split blocks in the explicit list. It is only done
 * when needed. A free block contains its size and a pointer to the 
 * next free block.
 * Essentially, this implementation is a variant of "segregated free 
 * lists" with a tuned size classes for the test program, "mdriver".
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <string.h>

#include "mm.h"
#include "memlib.h"

/*********************************************************
 * NOTE TO STUDENTS: Before you do anything else, please
 * provide your team information in the following struct.
 ********************************************************/
team_t team = {
    /* Team name */
    "b. bonny",
    /* First member's full name */
    "Phyo Thiha",
    /* First member's email address */
    "pthiha@cs.rochester.edu",
    /* Second member's full name (leave blank if none) */
    "",
    /* Second member's email address (leave blank if none) */
    ""
};

/* single word (4) or double word (8) alignment */
#define ALIGNMENT 8

/* rounds up to the nearest multiple of ALIGNMENT */
#define ALIGN(size) (((size) + (ALIGNMENT-1)) & ~0x7)

/* helper macros */
#define SIZE_T_SIZE (ALIGN(sizeof(int)))
#define GET(p) (*(unsigned int *)(p))
#define PUT(p, val) (*(unsigned int *)(p) = (val))
#define GET_SIZE(p) (GET(p) & ~1)
#define GET_ALLOC(p) (GET(p) & 0x1)

/* block structure for freeing and allocating blocks */
typedef struct block block;
struct block
{
	int size;
	block* next;
};

/* function prototypes */
void add(block* b);
void remov(block* b);
block* split(block* b, int splitPt);
int getSize(int list_index);
int getList(int size);
block* find(int size);
block* allocate(int size);

/* free list sizes */
int listSizes[10] = {16, 24, 40, 72, 136, 264, 520, 1032, 2056, 0};

/* pointer to beginning of free lists */
block* freeLists[10];

/* consistency checker */
int mm_check();

/* add - Adds a block to the appropriate free list */
void add(block* b)
{
	int i = getList(b->size);
	block* prev = freeLists[i];

	if (b < prev || prev == 0 || i != 9)
	{
		b->next = freeLists[i];
		freeLists[i] = b;
	}
	else		// linked list push
	{
		while (prev->next && b > prev->next)
			prev = prev->next;

		b->next = prev->next;
		prev->next = b;
	}
}

/* remov - Removes a block from its free list */
void remov(block* b)
{
	int i = getList(b->size);
	block* temp = freeLists[i];

	if (temp == b)
		freeLists[i] = temp->next;
	else
	{
		while (temp && temp->next != b)
			temp = temp->next;

		temp->next = b->next;
	}
}

/* split - Splits a block at a specified point */
block* split(block* b, int splitPt)
{
	block* split = (block*)((char*)b + splitPt);

	if ((b->size - splitPt) <= 2056)
		return b;

	split->size = b->size - splitPt;
	split->next = b->next;
	b->size = splitPt;
	b->next = split;
	
	return b;
}

/* getSize -  returns the size of the free list*/
int getSize(int list_index)
{
	return listSizes[list_index];
}

/* getList - Gets the index of the size list given a block size*/
int getList(int size)
{
	int i;
	
	if (size <= 16) i = 0;	// this could've been implemented as a hash if not in 'C'
	else if (size <= 24) i = 1;
	else if (size <= 40) i = 2;
	else if (size <= 72) i = 3;
	else if (size <= 136) i = 4;
	else if (size <= 264) i = 5;
	else if (size <= 520) i = 6;
	else if (size <= 1032) i = 7;
	else if (size <= 2056) i = 8;
	else i = 9;
	
	return i;
}

/* find - Finds a block that can fit the requested size*/
block* find(int size)
{
	block* b = freeLists[9];
	
	while (b)	// linked list walk through
	{
		while (b->size < size && b->next == (block*)((char*)b + b->size))
		{
			b->size += b->next->size;
			b->next = b->next->next;
		}
		
		if (b->size > size)
			return split(b, size);
		else if (b->size == size)
			return b;
		
		b = b->next;
	}
	
	return NULL;
}

/* allocate - Allocates a block that fits the requested size */
block* allocate(int size)
{
	int i = getList(size);
	int block_size = getSize(i);
	block* b;
	
	if (block_size == 0)
		block_size = size;

	b = mem_sbrk(block_size);	// increase the heap limit

	if ((int)b == -1) 
		return NULL;

	b->size = block_size & ~1;
	b->next = NULL;

	return b;
}

/* mm_init - Initialize the malloc package. Makes the free lists empty. */
int mm_init(void)
{
	int i;

	for (i = 0; i < 10; i++)
	{
		freeLists[i] = NULL;
		if (i < 3)
		{
			add(allocate(getSize(i)));
			add(allocate(getSize(i)));
		}
	}
	
	return 0;
}

/* mm_malloc - Allocate a block from an appropriate free list, else allocates a new block. */
void *mm_malloc(size_t size)
{
	int newSize = ALIGN(size + SIZE_T_SIZE);
	int i = getList(newSize);
	block* b = freeLists[i];
	
	if (size <= 0)
		return NULL;
	
	if (i == 9)
		b = find(newSize);
	
	if (b == 0)
		b = allocate(newSize);
	else
		remov(b);

	if (b == 0)
		return NULL;

	b->size = 1 | b->size;

	return ((void*)((char*)b + SIZE_T_SIZE));
}

/* mm_free - Frees a block and adds it to the free list. */
void mm_free(void *p)
{
	block* b = (block*)((char*)p - SIZE_T_SIZE);

	if ((b->size & 1) == 0) exit(-1);

	b->size = ~1 & b->size;

	add(b);

}

/* mm_realloc - Extends a block if possible, else a new block is allocated with a copy of the data. */
void *mm_realloc(void *p, size_t size)
{
	void* new;
	int newSize = ALIGN(size + SIZE_T_SIZE);
	int i = getList(newSize);
	int copy;
	block* b = (block*)((char*)p - SIZE_T_SIZE);
	block* other;

	if (GET_SIZE(b) >= newSize)
		return p;

	if (i == 9)	// for sizes for above '2056'
	{
		other = (block*)((char*)b + GET_SIZE(b));
		if ((void*)other < mem_heap_hi() && !(other->size & 1))
		{
			if ((GET_SIZE(b) + other->size) > newSize)
				other = split(other, newSize - GET_SIZE(b));

			if ((GET_SIZE(b) + other->size) == newSize)
			{
				b->size += other->size;
				remov(other);

				return ((void*)((char*)b + SIZE_T_SIZE));
			}
		}

		if ((void*)(block*)((char*)b + GET_SIZE(b)) >= mem_heap_hi())
		{
			new = mem_sbrk(newSize - GET_SIZE(b));

			if ((int)b == -1) 
				return NULL;

			b->size += newSize - GET_SIZE(b);

			return ((void*)((char*)b + SIZE_T_SIZE));
		}
	}

	new = mm_malloc(size);

	if (new == 0)			// if allocation failed
		return NULL;

	other = (block*)((char*)new - SIZE_T_SIZE);
	copy = GET_SIZE(other) - SIZE_T_SIZE;

	if (size < copy)		// check the out-of-bound allocations
		copy = size;

	memcpy(((void*)((char*)other + SIZE_T_SIZE)), p, copy);

	mm_free(p);

	return new;	
}

/* mm_check - Scans the heap and makes sure that everything is consistent. */

int mm_check()
{
	int i;
	block* b;
	int size = 0;

	for (i = 0; i < 10; i++)
	{
		b = freeLists[i];
		while (b)	// as long as the region is valid
		{
			if (b->size & 1)
			{
				printf("Free list contains block marked as used");
				return -1;
			}
			if (i != 9 && b->size != getSize(i))
			{
				printf("Wrong block size");
				return -1;
			}
			b = b->next;
		}
	}
	
	if (size != mem_heapsize())
	{
		printf("Wrong heap size");
		return -1;
	}
	
	return 0;
}
