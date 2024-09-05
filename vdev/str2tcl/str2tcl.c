#include <math.h>
#include <stdio.h>
#include <ctype.h>

#include "vicmain_c.h"
#include "applic.h"
#include "taeconf.inp"
#include "parblk.inc"
#include "defines.h"

#include "carto/cartoVicarProtos.h"
#include "carto/cartoStrUtils.h"
#include "carto/cartoTaeUtils.h"

/* prototypes */
int truncateToInteger(double a);
int isnumeric(char *str);

/*  convert string parm to TAE TCL var   A. Zobrist  9/20/04   */
/*  fixed floating point and illegal values  R. Bambery 10/01/2013  */
void main44(void)
{
   int ct,def,vtype,out=0,ret=0,int_result;
   double d=0.0,real_result;
   char inval[100];
   
   zifmessage("str2tcl - Oct 02, 2013 - (64-bit) - rjb");
   
   /* get the parameters */
   
   zvparm("inval",inval,&ct,&def,1,0);
   zvp("vtype",&vtype,&ct);

    ret = isnumeric(inval);
    
//    printf ("ret = %d\n",ret);

    if ( ret == 0) {
        if (vtype == 2 || vtype == 4) {
            out = -999;
        } else {
            d = -999.;
        }
    } else {
        d = strtod(inval, NULL);
//        printf("d = %le\n",d);
        if (vtype == 2 || vtype == 4) {
            out = truncateToInteger(d); 
//            printf("out = %d\n",out);
        }
    } 
   /* convert and output */
   
   switch (vtype)
      {
      case 2: 
      case 4: int_result = out;
              mq_out_int("val",int_result);
	      break;
      case 7: 
      case 8:       /*zmabend("real case need debugging"); */
//              printf("inval = %s\n",inval);
              // real_result = ms_dnum((char**)&inval);
                real_result = d;
//	      printf("real_result = %le\n",real_result);
              mq_out_real("val",real_result);
              break;
      }

   return;
}

int truncateToInteger(double a) {
    return (int) (a);
}
int isnumeric(char *str)
{
  while(*str)
  {
    if(!isdigit(*str)) {
      if (*str == '+' || *str == '-' || *str == '.' || *str == 'e')  {
      } else {
        return 0;
      }
    }
    str++;
  }

  return 1;
}
