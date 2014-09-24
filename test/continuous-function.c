#include <stdio.h>
#include <math.h>
#include <assert.h>

#define A	10	/* INT FASTENABLE */
#define B	1	/* BOOL FASTENABLE */
#define C	4	/* POW FASTENABLE */

int
main (void)
{
	assert (A > 7);
	assert (B == 0 || B == 1);
	assert (C && !(C & (C-1)));
	printf ("%f\n", fabs ((A-5.5)*(B+0.1)*(C-2.5)));
	return 0;
}
