/* Handy routines for Dumblang, written in C... */

#include <stdio.h>

void printIntC( int i )
{
   printf( "%d\n", i );
}

void printFloatC( float f )
{
   printf( "%f\n", f ); 
}

void printCharC( int i )
{
   printf( "%c", (char) i );
}

void printNLC()
{
   printf( "\n" );
}
