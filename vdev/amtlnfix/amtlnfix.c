#include <math.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

#include "carto/cartoMemUtils.h"
#include "carto/cartoVicarProtos.h"

/*  fix line-oriented blemish in AMT image data   A. Zobrist    10/04/05   */

void main44(void)
{
   int i,j,lnl,lns,i_unit,o_unit,cnt,def,status,blemwid;
   int blemln[2],b1t,b1b,b2t,b2b,mult,bsum,tsum;
   int sampoff[2],jlow,jtop;
   short int **buf;
   float div,b1fac,b2fac;
   
   zifmessage("amtlnfix version Wed Jan  2 2008");
   
   /* get some parms */
   
   zvparm("BLEMLN",blemln,&cnt,&def,2,0);
   zvparm("SAMPOFF",sampoff,&cnt,&def,2,0);
   zvp("BLEMWID",&blemwid,&cnt);
   mult = zvptst("MULT");
   
   /* open the files, input: read to half */
      
   status = zvunit(&i_unit,"INP",1, NULL);
   status = zvopen(i_unit,"OP","READ","U_FORMAT","HALF",
      "OPEN_ACT","SA","IO_ACT","SA", NULL);
   zvget(i_unit,"NL",&lnl,"NS",&lns, NULL);
      
   status=zvunit(&o_unit,"OUT",1, NULL);
   status=zvopen(o_unit,"U_NL",lnl,"U_NS",lns,"U_FORMAT","HALF",
     "OP","WRITE","OPEN_ACT","SA","IO_ACT","SA", NULL);
   
   /* dynamically allocate the buffers */
   
   mz_alloc2((unsigned char ***)&buf,lnl,lns,2);
   
   /* read the input */
   
   for (i=0;i<lnl;i++)
      zvread(i_unit,buf[i],"LINE",i+1,"SAMP",1,"NSAMPS",lns, NULL);
   
   /* get blemish statistics */
   
   jlow = sampoff[0];
   if (sampoff[1]!=0) jtop = jlow+sampoff[1]; else jtop = lns;
   
   b1t = blemln[0]-1;
   b1b = b1t+blemwid-1;
   b2t = blemln[1]-1;
   b2b = b2t+blemwid-1;

   bsum = 0.0;
   tsum = 0.0;
   div = 2.0/(float)lns;
   if (b1t>0&&b1t<lnl&&!(b1t>=b2t&&b1t<=b2b+1))
      {
      div /= 2.0;
      for (j=jlow;j<jtop;j++) { bsum += buf[b1t][j]-buf[b1t-1][j]; tsum += buf[b1t-1][j]; }
      }
      
   if (b1b>=0&&b1b<lnl-1&&!(b1b>=b2t&&b1b<=b2b+1))
      {
      div /= 2.0;
      for (j=jlow;j<jtop;j++) { bsum += buf[b1b][j]-buf[b1b+1][j]; tsum += buf[b1t+1][j]; }
      }
   if (mult) b1fac = (float)tsum/(float)(bsum+tsum+0.00001);
      else   b1fac = (float)bsum*div;
      
   bsum = 0.0;
   tsum = 0.0;
   div = 2.0/(float)lns;
   if (b2t>0&&b2t<lnl&&!(b2t>=b1t&&b2t<=b1b+1))
      {
      div /= 2.0;
      for (j=jlow;j<jtop;j++) { bsum += buf[b2t][j]-buf[b2t-1][j]; tsum += buf[b2t-1][j]; }
      }
      
   if (b2b>=0&&b2b<lnl-1&&!(b2b>=b1t&&b2b<=b1b+1))
      {
      div /= 2.0;
      for (j=jlow;j<jtop;j++) { bsum += buf[b2b][j]-buf[b2b+1][j]; tsum += buf[b2t+1][j]; }
      }
   if (mult) b2fac = (float)tsum/(float)(bsum+tsum+0.00001);
      else   b2fac = (float)bsum*div;
      
   /* apply blemish statistics */
   
   for (i=b1t;i<=b1b;i++)
      {
      if (i<0||i>=lnl) continue;
      for (j=0;j<lns;j++)
         if (mult) buf[i][j] *= b1fac; else buf[i][j] -= b1fac; 
      }

   for (i=b2t;i<=b2b;i++)
      {
      if (i<0||i>=lnl) continue;
      for (j=0;j<lns;j++)
         if (mult) buf[i][j] *= b2fac; else buf[i][j] -= b2fac; 
      }

   /* write out the result */
   
   for (i=0;i<lnl;i++)
      zvwrit(o_unit,buf[i],"LINE",i+1,"SAMP",1,"NSAMPS",lns, NULL);
   
   zvclose(i_unit, NULL);
   zvclose(o_unit, NULL);
   return;
}
