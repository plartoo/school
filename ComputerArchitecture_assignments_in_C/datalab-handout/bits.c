/* 
 * CS:APP Data Lab 
 * 
 * <Phyo Thiha (pthiha); Email: pthiha@cs.rochester.edu>
 * 
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.  
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:
 
  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code 
  must conform to the following style:
 
  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>
    
  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.

 
  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting an integer by more
     than the word size.

EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implent floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


NOTES:
  1. Use the dlc (data lab checker) compiler (described in the handout) to 
     check the legality of your solutions.
  2. Each function has a maximum number of operators (! ~ & ^ | + << >>)
     that you are allowed to use for your implementation of the function. 
     The max operator count is checked by dlc. Note that '=' is not 
     counted; you may use as many of these as you want without penalty.
  3. Use the btest test harness to check your functions for correctness.
  4. Use the BDD checker to formally verify your functions
  5. The maximum number of ops for each function is given in the
     header comment for each function. If there are any inconsistencies 
     between the maximum ops in the writeup and in this file, consider
     this file the authoritative source.

/*
 * STEP 2: Modify the following functions according the coding rules.
 * 
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the BDD checker to formally verify that your solutions produce 
 *      the correct answers.
 */


/* 
  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>
 * bitAnd - x&y using only ~ and | 
 *   Example: bitAnd(6, 5) = 4
 *   Legal ops: ~ |
 *   Max ops: 8
 *   Rating: 1
 */
#endif

int bitAnd(int x, int y) {

  /* Use DeMorgan's law, (not p) or (not q) == not(p or q) */

  return ~(~x | ~y);

}

/* 
 * byteSwap - swaps the nth byte and the mth byte
 *  Examples: byteSwap(0x12345678, 1, 3) = 0x56341278
 *            byteSwap(0xDEADBEEF, 0, 2) = 0xDEEFBEAD
 *  You may assume that 0 <= n <= 3, 0 <= m <= 3
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 25
 *  Rating: 2
 */
int byteSwap(int x, int n, int m) {

	/* 
	 * Steps - 
	 * 1. Turn on just nth and mth bytes in temp variables
	 * 2. Shift 'n' and 'm' bytes to the last spots
	 * 3. Clear 'n' and 'm' bytes in 'x'
	 * 4. Shift nth and mth byte over by 'm' and 'n' respectively
	 * 5. Merge two temps into 'x'
	 */

	int mask1 = 0xFF << (n << 3);
	int mask2 = 0xFF << (m << 3);
	
	int temp1 = x & mask1; // step 1
	int temp2 = x & mask2; // step 1
	
	temp1 = temp1 >> (n << 3) & 0xFF; // step 2
	temp2 = temp2 >> (m << 3) & 0xFF; // step 2
	
	x = x & ~(mask1 | mask2); // step 3
	
	temp1 = temp1 << (m << 3); // step 4
	temp2 = temp2 << (n << 3); // step 4
	
	return x^(temp1 | temp2); // step 5

}

/* 
 * logicalShift - shift x to the right by n, using a logical shift
 *   Can assume that 0 <= n <= 31
 *   Examples: logicalShift(0x87654321,4) = 0x08765432
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3 
 */
int logicalShift(int x, int n) {

	/* 
	 * Steps - 
	 * 1. Create an integer with a 1 followed by 31 0's (resulting in temp1)
	 * 2. Right-shift by 'n' bits
	 * 3. Set temp2 such that temp1 is shifted right with all leading bits filled with 1s
	 * 4. Use '&' to filter the bit that BOTH have 1s
	 */

	int temp1, temp2;
	temp1 = 0x01 << 31; // step 1
	x = x >> n; // step 2
	temp2 = temp1 >> n; // step 3
	
	return x & ~(temp2 << 1); // step 4

}

/*
 * bitParity - returns 1 if x contains an odd number of 0's
 *   Examples: bitParity(5) = 0, bitParity(7) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 4
 */
int bitParity(int x) {
	/*
	 * We take advantage of the fact that 
	 * XOR will return true only if the two bits 
	 * being compared are not both 0 and 1.  
	 *
	 * The algorithm breaks the number down into 
	 * simpler cases, reducing by half each time, until 
	 * there is only the one bit left to compare.  
	 *
	 * If the remaining bit is a 1, then the number 
	 * of 0s is odd from the original number
	 */

	x = (x>>16) ^ x;
	x = (x>>8) ^ x;
	x = (x>>4) ^ x;
	x = (x>>2) ^ x;
	x = (x>>1) ^ x;
	return (x & 1);

}

/* 
 * logicalNeg - implement the ! operator, using all of 
 *              the legal operators except !
 *   Examples: logicalNeg(3) = 0, logicalNeg(0) = 1
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 4 
 */
int logicalNeg(int x) {

	/*
	 * The complement of 0 is 0.  
	 * If a value is not 0, its complement will 
	 * be positive or negative.
	 *
	 * When negative | x is evaluated, the most 
	 * significant bit is a 1 because either the 
	 * value or its complement must
	 * be negative.  To ensure that the opposite 
	 * value is returned, the result will be shifted 
	 * all the way around the loop (by 31) and 1 
	 * will be added to make sure it's the correct value 
	 */

	int negative = ~x + 1;
	return ((~(negative | x)) >> 31) & 1;


}

/* 
 * tmin - return minimum two's complement integer 
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 4
 *   Rating: 1
 */
int tmin(void) {

	/* 
	 * The min value for a two's complement integer is 
	 * a 1 followed by all zeros. Therefore, because this
	 * is a 32-bit machine, if the integer is shifted by 
	 * 28 to the left, the only thing that remains is
	 * the last 4 bits, or the last and smallest byte 
	 */

  return (0x08<<28);

}

/* 
 * sign - return 1 if positive, 0 if zero, and -1 if negative
 *  Examples: sign(130) = 1
 *            sign(-23) = -1
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 10
 *  Rating: 2
 */
int sign(int x) {

	/*
	 * Shifting x>>31 will return all 0s 
	 * if the operand is positive & all 1s 
	 * if the operand is negative
	 *
	 * This is similar to the isPositive function, 
	 * so the result will only be 0 if both shift & !!x are 0.
	 */

	int shift = x>>31; //0s if positive, 1s if negative
	return shift | (!!x);
}

/* 
 * divpwr2 - Compute x/(2^n), for 0 <= n <= 30
 *  Round toward zero
 *   Examples: divpwr2(15,1) = 7, divpwr2(-33,4) = -2
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 2
 */
int divpwr2(int x, int n) {

	/*
	 * For two's-complement machines using arithmatic right shifts, 
	 * x/(2^n) can be computed and rounded correctly using the conditional 
	 * statement: (x<0)? x+(1<<n)-1 : x) >> n.
	 *
	 * Using bitwise operations, the conditional statement a ? b : c is 
	 * equivalent to (a & b) ! (~a & c) which is used below
	 */

	int bias = (x>>31) & (-1);
	int power = x + (1 << n) - 1;
	return ( (bias & power) | (~bias & x)) >> n;

}

/* 
 * negate - return -x 
 *   Example: negate(1) = -1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 5
 *   Rating: 2
 */
int negate(int x) {

	/* 
	 * Switch all the bits (zeros become ones and vice-versa)
	 * and then adding a 1.
	 */

	return ~x + 1;

}

/* 
 * isPositive - return 1 if x > 0, return 0 otherwise 
 *   Example: isPositive(-1) = 0.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 3
 */
int isPositive(int x) {

	/*
	 * If the number is positive, shifting x>>31 
	 * will make all the values will fill in as 0, 
	 * but if it is negative, all the values will 
	 * fill in with the most significant bit, or 1.
	 *
	 * To determine if the shift is all 0s or all 1s, 
	 * check the OR with !x. For the number to be
	 * positive and all 0s, the only way this can be 
	 * true is if shift and !x are all 0s.  This 
	 * will result in false, meaning it is positive, 
	 * so the function should return !result, or true.
	 */

	int shift = x>>31;
	int result = shift | (!x);
	return !result;
}

/* 
 * isLessOrEqual - if x <= y  then return 1, else return 0 
 *   Example: isLessOrEqual(4,5) = 1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 24
 *   Rating: 3
 */
int isLessOrEqual(int x, int y) {

	/** Determine the sign of x and y */
	int signx = x>>31; // will return all 0's if positive, all 1's if negative
	int signy = y>>31; // will return all 0's if positive, all 1's if negative
	
	int sign = signx^signy; // determine if x and y are the same sign; 0 if both pos or neg, 1 if one pos & one neg
	
	/* case1: if both x & y are pos or neg, 
	 * then return the opposite boolean for the 
	 * sign of y; else, return 0 to indicate false
	 *
	 * This uses the fact that if these have opposite 
	 * signs, whether x is less than or equal to y is 
	 * the same as !signy
	 */

	int case1 = (sign & !signy) | (~sign & 0);
	
	/** subtract y - x*/
	int ysubx = (y + (~x + 1));
	
	/* If x and y have the same sign, then the 
	 * correct boolean value will depend on the 
	 * sign of the subtraction.
	 *
	 * Else, return false, indicating that x & y 
	 * do not have the same sign
	 */

	int case2 = (~sign & !(~sign & (ysubx>>31))) | (~(~sign) & 0);
	
	return case1 | case2;
}

/*
 * ilog2 - return floor(log base 2 of x), where x > 0
 *   Example: ilog2(16) = 4
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 90
 *   Rating: 4
 */
int ilog2(int x) {

	/* 
	 * Count how many positions right we can shift the number before it's 0.
	 * Could've done with for/while loop, but am not allowed to.
	 */

	int ret, temp;
	temp = x >> 16;
	ret = (!!temp << 4);
	x = x >> (!!temp << 4);

	temp = x >> 8;
	ret = ret + (!!temp << 3);
	x = x >> (!!temp << 3);

	temp = x >> 4;
	ret = ret + (!!temp << 2);
	x = x >> (!!temp << 2);

	temp = x >> 2;
	ret = ret + (!!temp << 1);
	x = x >> (!!temp << 1);

	temp = x >> 1;
	ret = ret + (!!temp);

	return ret;

}

/* 
 * float_neg - Return bit-level equivalent of expression -f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   When argument is NaN, return argument.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 10
 *   Rating: 2
 */
unsigned float_neg(unsigned uf) {

	/* 
	 * The first bit in a floating point 
	 * number represents the sign, so only 
	 * the first bit needs to be flipped to express -f
	 */

	return uf^(1<<31);

}

/* 
 * float_i2f - Return bit-level equivalent of expression (float) x
 *   Result is returned as unsigned int, but
 *   it is to be interpreted as the bit-level representation of a
 *   single-precision floating point values.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned float_i2f(int x) {

	unsigned power, round;

	/* 1 as most significant bit (will be used as sign bit) */
	unsigned sig_bit = 1 << 31;

	/* base case */
	unsigned sign = 0;
	unsigned unsigned_x = x;

	/* when 'x' is zero */
	if(x == 0)
		return 0;

	/* when 'x' is negative, 
	 * change the sign and negate unsigned_x */
	if(x < 0) {
		sign = sig_bit;
		unsigned_x = -x;
	}


	power = 150; // power == bias + size of significant

	/* case 1: shift until unsigned_x fits to the right */
	while(unsigned_x < (1 << 23)) {
		unsigned_x <<= 1;
		power -= 1;
	}
	
	/* case 2: complement of case 1 */
	while(unsigned_x >= (1 << 24)) {
		round = 0;

		/* shift down, while unsigned_x does not fit significand */
		while(unsigned_x >= (1 << 24)) {

			round >>= 1; // shift the round down

			/* round it up when lowest bit is set */
			if(unsigned_x & 1) {
				round |= sig_bit;
			}

			/* shift unsigned_x down and repeat */
			unsigned_x >>= 1;
			power += 1;
		}

		/* 
		 * when round >= 1/2 and unsigned_x's lowest bit is
		 * set, add one to the result
		 */
		if(round > sig_bit || (round == (sig_bit) && (unsigned_x & 1))) {
			unsigned_x += 1;
		}

	}

	unsigned_x = unsigned_x & ((1 << 23) - 1); // get rid of leading bits

	return sign | power << 23 | unsigned_x; // join all up for final float number

}

/* 
 * float_twice - Return bit-level equivalent of expression 2*f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representation of
 *   single-precision floating point values.
 *   When argument is NaN, return argument
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned float_twice(unsigned uf) {

	int negative_zero, not_a_num;
	int exp, fraction, empty;

	negative_zero = 0x80000000;
	not_a_num = 0x7fc00000;

	// required by problem statement or just common sense
	if(uf == negative_zero || uf == 0 || uf == not_a_num){
		return uf;
	}

	// shift right and mask
	exp=((uf >> 23) & 0xff);

	/*
	 * when above is zero, multiply the fraction part while
	 * leaving the exponent part intact.
	 */
	if(exp == 0){

		fraction = ((0x7FFFFF) & uf) << 1;
		empty = ~(0x7FFFFF) & uf;

		return fraction | empty;

	}else{

		if(exp == 0xff) // for +/- INF case
			return uf;

		// add one to exp and shift down, and join with empty for return
		exp = exp + 1;
		exp = exp << 23;
		empty = ~(0xff << 23) & uf;

		return empty | exp;
	}


}

