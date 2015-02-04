/**
 * cs456
 * Header file to store the prototype of page_ref_timer interrupt handler
 *
 */

#ifndef PAGEREFTIMER_H
#define PAGEREFTIMER_H

#include <linux/types.h>
#include <linux/spinlock.h>
#include <asm/atomic.h>


/**
 * Update the timers of each pages on the timer timeout
 */
int page_ref_timer_callback(void);


#endif /* PAGEREFTIMER_H */
