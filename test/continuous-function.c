#include <stdio.h>
#include <math.h>

#define A	10	/* FASTENABLE */
#define B	3	/* FASTENABLE */
#define C	5	/* FASTENABLE */

int
main (void)
{
	printf ("%f\n", fabs ((A-5.5)*(B-8.5)*(C*2.5)));
	return 0;
}
