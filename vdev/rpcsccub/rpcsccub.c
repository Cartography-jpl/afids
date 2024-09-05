#include <math.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "carto/cartoMemUtils.h"
#include "carto/cartoVicarProtos.h"

/************************************************************************/
/* program rpcsccub                                                      */
/************************************************************************/
/*  05-12 ...alz... initial version                     */
/*  Fri Dec 28 2007 wlb switched to USES_ANSI_C AND LIB_CARTO; misc cleanup */
/************************************************************************/

void main44(void)
{
   int i,j,dcols[5],scol,unit,colcount,coldef,ibis,status,clen;
   int parmcount,parmdef;
   int reord[5] = {9,8,10,6,7};
   double *buf,*outbuf,vmin,vmax,offset,scale;
   char rpctype[2];
   
   zifmessage("rpcsccub version Fri Dec 28 2007");
   
   /* get the basic parameters */
   
   zvparm("dcols",dcols,&colcount,&coldef,5,0);
   zvparm("scol",&scol,&colcount,&coldef,1,0);
   zvparm("rpctype",rpctype,&parmcount,&parmdef,1,2);
   
   /* read in points from the ibis interface file */

   status = zvunit(&unit,"inp",1, NULL);
   status = IBISFileOpen(unit,&ibis,"update",0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nr",&clen,1,1,0);
   
   mz_alloc1((unsigned char **)&buf,clen,8);
   mz_alloc1((unsigned char **)&outbuf,clen,8);
   
   /* process the scales and offsets */
   /* 6=line_off,7=samp_off,8=lat_off,9=lon_off,10=h+off */
   /* 11=line_sc,12=samp_sc,13=lat_sc,14=lon_sc,15=h+sc */
   
   for (i=0;i<clen;i++) outbuf[i] = 0.0;
   for (i=0;i<5;i++)
      {
      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",dcols[i]);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnRead(ibis,(char*)buf,dcols[i],1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
      vmin = buf[0]; vmax = vmin;
      for (j=0;j<clen;j++)
         {
         if (buf[j]>vmax) vmax = buf[j];
         if (buf[j]<vmin) vmin = buf[j];
         }
      offset = 0.5*(vmin+vmax);
      scale = 0.5*(vmax-vmin);
      printf("raw offset,scale %f %f\n",offset,scale);
      if (i<2)
         {
         offset = ((float)((int)(offset*10000.0)))/10000.0;
         scale = ((float)((int)(scale*10000.0)))/10000.0;
         }
      else
         {
         offset = (float)(int)offset;
         scale = (float)(int)scale;
         }
      printf("truncated offset,scale %f %f\n",offset,scale);
      outbuf[reord[i]-1] = offset;
      outbuf[reord[i]+4] = scale;
      }
   printf("rpctype .%s.\n",rpctype);
   if (rpctype[0]=='A') outbuf[0] = 0; else outbuf[0] = 1;
   
   /* write the output column */
   
   status = IBISColumnWrite(ibis,(char*) outbuf,scol,1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISFileClose(ibis,0);
   if (status!=1) IBISSignal(ibis,status,1);
  
   return;
       
}
