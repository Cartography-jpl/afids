#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <string.h>
#include <ctype.h>

#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "carto/cartoStrUtils.h"
#include "carto/cartoGtUtils.h"
#include "carto/cartoMemUtils.h"
#include "carto/cartoVicarProtos.h"

/************************************************************************/
/* program rpcfwd                                                      */
/************************************************************************/
/*  99-08 ...alz... initial version                     */
/*  Fri Dec 28 2007 wlb switched to USES_ANSI_C AND LIB_CARTO; misc cleanup */
/************************************************************************/

void rpcrd(i,j,labelstr,val)
   int i,j;
   char *labelstr;
   double *val;
{
   char *p,rpcfield[15],numstr[5];
   
   strcpy(rpcfield,"RPC_FIELD");
   if (i>0)
      {
      sprintf(numstr,"%d",i);
      strcat(rpcfield,numstr);
      }
   sprintf(numstr,"%d=",j+1);
   strcat(rpcfield,numstr);
   p = ms_find(labelstr,rpcfield);
   val[j] = ms_dnum(&p);
   /*printf("rpcfield .%s. val[j] %25.18f\n",rpcfield,val[j]);*/

   return;
}

double rpceval(isline,lon,lat,elv,rpck,rpcn,rpcd,rpctype)
   int isline,rpctype;
   double lon,lat,elv,rpck[13],rpcn[20],rpcd[20];
{
   double l,p,h,l2,l3,p2,p3,h2,h3,numer,denom;
   
   l = (lon-rpck[6])/rpck[11];
   p = (lat-rpck[5])/rpck[10];
   h = (elv-rpck[7])/rpck[12];
   l2 = l*l; l3 = l2*l;
   p2 = p*p; p3 = p2*p;
   h2 = h*h; h3 = h2*h;
   
   if (rpctype==0)       /* type RPC00A */
      {
      numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
      rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*p*l*h+
      rpcn[8]*l2+rpcn[9]*p2+rpcn[10]*h2+rpcn[11]*l3+
      rpcn[12]*l2*p+rpcn[13]*l2*h+rpcn[14]*l*p2+
      rpcn[15]*p3+rpcn[16]*p2*h+rpcn[17]*l*h2+
      rpcn[18]*p*h2+rpcn[19]*h3;
      denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
      rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*p*l*h+
      rpcd[8]*l2+rpcd[9]*p2+rpcd[10]*h2+rpcd[11]*l3+
      rpcd[12]*l2*p+rpcd[13]*l2*h+rpcd[14]*l*p2+
      rpcd[15]*p3+rpcd[16]*p2*h+rpcd[17]*l*h2+
      rpcd[18]*p*h2+rpcd[19]*h3;
      }
   else                   /* type RPC00B */
      {
      numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
      rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*l2+
      rpcn[8]*p2+rpcn[9]*h2+rpcn[10]*p*l*h+rpcn[11]*l3+
      rpcn[12]*l*p2+rpcn[13]*l*h2+rpcn[14]*l2*p+
      rpcn[15]*p3+rpcn[16]*p*h2+rpcn[17]*l2*h+
      rpcn[18]*p2*h+rpcn[19]*h3;
      denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
      rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*l2+
      rpcd[8]*p2+rpcd[9]*h2+rpcd[10]*p*l*h+rpcd[11]*l3+
      rpcd[12]*l*p2+rpcd[13]*l*h2+rpcd[14]*l2*p+
      rpcd[15]*p3+rpcd[16]*p*h2+rpcd[17]*l2*h+
      rpcd[18]*p2*h+rpcd[19]*h3;
      }

   
   if (isline) return(rpck[8]*numer/denom+rpck[3]);
      else     return(rpck[9]*numer/denom+rpck[4]);
}

void main44(void)
{
  int i,j,cols[5],unit,colcount,coldef,rpctype;
   int ibis,status,clen,labnl,labns,misselv,elvct;
   char *labelstr,*p;
   double *lon,*lat,*elev,*line,*samp,avgelv;
   double rpck[13],rpcln[20],rpcld[20],rpcsn[20],rpcsd[20];
   
   zifmessage("rpcfwd version Fri Dec 28 2007");
   
   /* get the basic parameters */
   
   zvparm("cols",cols,&colcount,&coldef,5,0);
   if (colcount!=5) zmabend("Requires five columns");
   printf("converting columns (%d,%d,%d) to columns (%d,%d)\n",cols[0],
       cols[1],cols[2],cols[3],cols[4]);
   misselv = zvptst("misselv");
   
   /* read the rpc's from the second input */
   
   status = gtgetlab("inp",2,&labelstr,&labnl,&labns);
   for (j=0;j<13;j++) rpcrd(0,j,labelstr,rpck);
   for (j=0;j<20;j++) rpcrd(14,j,labelstr,rpcln);
   for (j=0;j<20;j++) rpcrd(15,j,labelstr,rpcld);
   for (j=0;j<20;j++) rpcrd(16,j,labelstr,rpcsn);
   for (j=0;j<20;j++) rpcrd(17,j,labelstr,rpcsd);
   
   p = ms_find(labelstr,"NITF_CETAG=");

   rpctype = 0;

   if (p!=0)
      {
      if (strncmp(p,"RPC00A",6)==0)
         {
         rpctype = 0;
         printf("processing RPC00A\n");
         }
      if (strncmp(p,"RPC00B",6)==0)
         {
         rpctype = 1;
         printf("processing RPC00B\n");
         }
      }
   else
      {
      rpctype = 0;
      printf("processing RPC00A by default\n");
      }
      
   /* read in points from the ibis interface file */

   status = zvunit(&unit,"inp",1, NULL);
   status = IBISFileOpen(unit,&ibis,"update",0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nr",&clen,1,1,0);
   mz_alloc1((unsigned char **)&lon,clen,8);
   mz_alloc1((unsigned char **)&lat,clen,8);
   mz_alloc1((unsigned char **)&elev,clen,8);
   mz_alloc1((unsigned char **)&line,clen,8);
   mz_alloc1((unsigned char **)&samp,clen,8);

   for (i=0;i<5;i++)
      {
      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[i]);
      if (status!=1) IBISSignal(ibis,status,1);
      }
   
   status = IBISColumnRead(ibis,(char*) lon,cols[0],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*) lat,cols[1],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*) elev,cols[2],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);

   /* calculate the elevation average */
  
   avgelv = 0.0;
   if (misselv)
      {
      elvct = 0;
      for (i=0;i<clen;i++)
         {
         if (elev[i]<-990.0) continue;
         avgelv += elev[i];
         elvct++;
         }
      if (elvct==0) zmabend("MISSING ELEVATION, CHECK YOUR DTED");
      avgelv /= (double)elvct;
      }
   
   /* calculate the output data, line-samp in VICAR coord, rpc in area coord */
   
   for (i=0;i<clen;i++)
      {
      if (lon[i]>180.0) lon[i] = lon[i]-360.0;
      if (elev[i]<-990.0)
         {
         if (misselv) elev[i] = avgelv;
         else
            {
            printf("lon,lat,elev %f %f %f\n",lon[i],lat[i],elev[i]);
            zmabend("MISSING ELEVATION, CHECK YOUR DTED");
            }
         }
      line[i] = rpceval(1,lon[i],lat[i],elev[i],rpck,rpcln,rpcld,rpctype)+0.5;
      samp[i] = rpceval(0,lon[i],lat[i],elev[i],rpck,rpcsn,rpcsd,rpctype)+0.5;
      }
   
   /* Output points to the ibis interface file */
   
   status = IBISColumnWrite(ibis,(char*) line,cols[3],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnWrite(ibis,(char*) samp,cols[4],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   status = IBISFileClose(ibis,0);
   if (status!=1) IBISSignal(ibis,status,1);
}
