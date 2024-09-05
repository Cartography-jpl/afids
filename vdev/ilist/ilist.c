#include <math.h>
#include <stdio.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "carto/cartoVicarProtos.h"
#include "carto/cartoMemUtils.h"

#define MAXCOLS 500

/************************************************************************/
/* program ilist                                                      */
/************************************************************************/
/*  05-07 ...alz... initial version                     */
/************************************************************************/

void main44(void)
{
   int icol,unit,ibis,ncol,clen,cols[99],colcount,coldef;
   int status,labcount,labdef,iseq,row,dummy,ival,nrow,ibig;
   int prnt_rec;
   double val;
   char coltype[MAXCOLS][6],labels[MAXCOLS][50],lab[51];
   
   zifmessage("ilist version Wed Jan  2 2008");
   
   /* get parameters */
   
   zvparm("cols",cols,&colcount,&coldef,99,0);
   zvparm("labels",labels,&labcount,&labdef,MAXCOLS,50);
   zvp("sr",&row,&dummy);
   zvp("nr",&nrow,&dummy);
   prnt_rec = zvptst("prnt_rec");
   
   /* open the ibis interface file */

   status = zvunit(&unit,"inp",1, NULL);
   status = IBISFileOpen(unit,&ibis,IMODE_READ,0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nr",&clen,1,1,0);
   IBISFileGet(ibis,"nc",&ncol,1,1,0);
   IBISFileGet(ibis,"formats",coltype,1,MAXCOLS,6);
   if (nrow>(clen-row+1)) nrow = clen-row+1;
   
   /* loop over the cols to be printed */
   
   for (ibig=0;ibig<nrow;ibig++)
      {
      printf("\n");
      if (prnt_rec) printf("%20s = %d\n","record number",row+ibig);
      for (icol=0;icol<colcount;icol++)
         {
         iseq = cols[icol];
         status = IBISColumnGet(ibis,"FORMAT",coltype[iseq-1],iseq);
         if (status!=1) IBISSignal(ibis,status,1);
         if (coltype[iseq-1][0]=='A')     /* alpha column */
            {
            status = IBISColumnRead(ibis,lab,iseq,row+ibig,1);
            if (status!=1) IBISSignal(ibis,status,1);
            printf("%20s = %s\n",labels[icol],lab);
            }
         else if (coltype[iseq-1][0]=='D'||coltype[iseq-1][0]=='R')     /* real column */
            {
            status = IBISColumnRead(ibis,(char*) &val,iseq,row+ibig,1);
            if (status!=1) IBISSignal(ibis,status,1);
            printf("%20s = %f\n",labels[icol],val);
            }
         else                             /* int column */
            {
            status = IBISColumnRead(ibis,(char*) &ival,iseq,row+ibig,1);
            if (status!=1) IBISSignal(ibis,status,1);
            printf("%20s = %d\n",labels[icol],ival);
            }
         }
      }
   
   printf("\n"); 
   status = IBISFileClose(ibis,0);
   if (status!=1) IBISSignal(ibis,status,1);
   return;
}
