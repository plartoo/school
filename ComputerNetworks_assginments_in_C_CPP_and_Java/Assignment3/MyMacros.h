#ifndef __MY_MACROS_
#define __MY_MACROS_
#define WINDOW_SIZE 15
#define TIME_OUT_TIME 0.4

/*
 * A byte for size
 * 2 byte for seq#
 * 2 byte for ACK seq#
 * A byte for checksum
 */
#define HEADER_SIZE 7
#define SIZE_POSITION 0
#define SEQ_NUMBER_POSITION 1
#define ACK_NUMBER_POSITION 3
#define CHECKSUM_POSITION   5
#define MAX_BODY_SIZE 57

#define GET_SIZE(p)       ((unsigned char)p->data[SIZE_POSITION] & 0xFF)
#define GET_ACK_NUMBER(p) ((unsigned short) (((unsigned short)p->data[ACK_NUMBER_POSITION] & 0xFF) << 8) + ((unsigned short)p->data[ACK_NUMBER_POSITION+1] & 0xFF))
#define GET_SEQ_NUMBER(p) ((unsigned short) (((unsigned short)p->data[SEQ_NUMBER_POSITION] & 0xFF) << 8) + ((unsigned short)p->data[SEQ_NUMBER_POSITION+1] & 0xFF))
#define GET_CHECKSUM(p)   ((unsigned short) (((unsigned short)p->data[CHECKSUM_POSITION]   & 0xFF) << 8) + ((unsigned short)p->data[CHECKSUM_POSITION+1] & 0xFF))
#define SET_SIZE(p,size)   p->data[SIZE_POSITION] = ((char)size)
#define SET_SEQ(p,seq)     p->data[SEQ_NUMBER_POSITION]   = (char)(seq >> 8) & 0xFF;\
						   p->data[SEQ_NUMBER_POSITION+1] = (char)(seq & 0xFF)
#define SET_ACK(p,ack)     p->data[ACK_NUMBER_POSITION] = (char)((ack >> 8) & 0xFF);\
						   p->data[ACK_NUMBER_POSITION+1] = (char)(ack & 0xFF)

#define SET_CHECKSUM(p,check) p->data[CHECKSUM_POSITION] = ((check >> 8) & 0xFF);\
						   p->data[CHECKSUM_POSITION+1] = (char)(check & 0xFF)
#endif
