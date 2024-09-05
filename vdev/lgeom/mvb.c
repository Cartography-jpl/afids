#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>  /* memcpy, memset */
//#include <stdio.h>

void FTN_NAME(mvb)(char *from_bbuf, char *to_bbuf,int *nb);
void FTN_NAME(zba)(char *bbuf,int *nb);

/************************************************************************
 * MVB (move_a_byte_array)
 *     Fortran-Callable Version (no C-version needed -- use memcpy) 
 ************************************************************************/
void FTN_NAME(mvb)(from_bbuf,to_bbuf,nb)
char *from_bbuf, *to_bbuf;
int *nb;
{
//  printf ("MVB number of bytes = %d\n",*nb);
  memcpy(to_bbuf,from_bbuf,(size_t)*nb);
  return;
}

/************************************************************************
 * ZBA (zero_a_byte_array)
 *     Fortran-Callable Version (no C-version needed -- use memset) 
 ************************************************************************/
void FTN_NAME(zba)(bbuf,nb)
char *bbuf;
int *nb;
{
  memset(bbuf,0,(size_t)*nb);
  return;
}
