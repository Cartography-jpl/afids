#include <math.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "taeconf.inp"
#include "parblk.inc"
#include "defines.h"

#include "carto/cartoVicarProtos.h"
#include "carto/cartoStrUtils.h"
#include "carto/cartoTaeUtils.h"

/*  get substring  into TAE TCL var   R. Bambery 10/02/2013   */
void main44(void)
{
   int ct,def,stchar,numchar;        
   char instr[100],substr[100];
   
   zifmessage("substr2tcl - Oct 02, 2013 - (64-bit) - rjb");
   
   /* get the parameters */
   
   zvparm("instring",instr,&ct,&def,1,0);
   zvp("stchar",&stchar,&ct);
   zvp("numchar",&numchar,&ct);

//    printf ("numchar = %d\n",numchar);
//    printf ("stchar = %d\n",stchar);
//    printf("%.*s\n", numchar, instr + stchar-1);
//    instr[stchar+numchar+1] = '\0';
    strncpy(substr, instr+stchar-1, (long unsigned int)(numchar)+1);
    substr[numchar] = '\0';
//    printf ("substr = %s\n",substr);    
//    sprintf(substr, instring, instr + stchar);

    mq_out_string("substr",substr,100);
   return;
}

