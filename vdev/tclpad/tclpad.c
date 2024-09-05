#include <math.h>
#include <stdio.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "taeconf.inp"
#include "parblk.inc"
#include "defines.h"

#include "carto/cartoTaeUtils.h"
#include "carto/cartoVicarProtos.h"

#define MAXSTRPAR  130

/*  pad a TAE TCL var   A. Zobrist  11/29/05   */

void main44(void)
{
   int i,ct,def,len,padleft,padlen;
   char inval[102],pchar[3],outval[102],pad[102];
   
   zifmessage("tclpad version Sat Dec 29 2007");
   
   /* get the parameters */
   
   zvparm("inval",inval,&ct,&def,1,0);
   zvp("len",&len,&ct);
   zvparm("char",pchar,&ct,&def,1,0);
   padleft = zvptst("left");
   if (len>99) zmabend("output len limited to 99");
   
   /* convert and output */
   
   padlen = len-strlen(inval);
   for (i=0;i<padlen;i++) pad[i] = pchar[0];
   pad[padlen] = (char)0;
   if (padleft)
      {
      strcpy(outval,pad);
      strcat(outval,inval);
      }
   else
      {
      strcpy(outval,inval);
      strcat(outval,pad);
      }
   mq_out_string("val",outval,MAXSTRPAR);

   return;
}
