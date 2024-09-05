#include <math.h>
#include <stdio.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "taeconf.inp"
#include "parblk.inc"
#include "defines.h"

#include "carto/cartoVicarProtos.h"
#include "carto/cartoMemUtils.h"
#include "carto/cartoTaeUtils.h"

/*  linear shift using non-fourier correlation   A. Zobrist    08/24/04   */

void main44(void)
{
   int i,j,apodize,subpix,unit1,nl1,ns1,unit2,nl2,ns2,pcnt,pdef,status;
   int ncor,rline1,rline2,rptr1,rptr2,qline,ixs,nshftmax,ishft,imax=0;
   int xnum,jmax=0,pcount,zthresh[2];
   short int **buf1,**buf2;
   float *cx1,*cy1,*cv1,**cx2,**cy2,**cv2,dline,dsamp,dist;
   float fl,fs,top,bot,sum,smax,avg,pval,pval2,zl,zu;
   double shiftmax,itie[6],otie[6],xdelta[2];
   char fmt_str[10];
   
   zifmessage("linshft version Wed Jan  2 2008");
   
   /* get some parms */
   
   apodize = zvptst("APODIZE");
   subpix = zvptst("SUBPIX");
   zvparmd("SHIFTMAX",&shiftmax,&pcnt,&pdef,1,0);
   zvparmd("ITIE",itie,&pcnt,&pdef,4,0);
   zvparmd("OTIE",otie,&pcnt,&pdef,4,0);
   zvparmd("XDELTA",xdelta,&pcnt,&pdef,2,0);
   zvparmd("ZTHRESH",zthresh,&pcnt,&pdef,2,0);
   zvparm("xnum",&xnum,&pcnt,&pdef,1,0);
   zl = (float)(zthresh[0])-0.01;
   zu = (float)(zthresh[1])+0.01;
   
   if (xnum>1&&itie[1]!=itie[3]) zmabend("at present cross case only for vertical line");
   
   /* open the image files */
   
   status = zvunit(&unit1, "INP", 1, NULL);
   status = zvopen(unit1, "OPEN_ACT", "SA", "IO_ACT", "SA",
			"U_FORMAT","HALF", NULL);
   zvget(unit1,"FORMAT",fmt_str, NULL);
   zvget(unit1,"NL",&nl1, NULL);
   zvget(unit1,"NS",&ns1, NULL);
   if ( strcmp(fmt_str,"BYTE") && strcmp(fmt_str,"HALF"))
      zmabend("Invalid input data format.  Use BYTE or HALF.");
   mz_alloc2((unsigned char ***)&buf1,2,ns1,2);
   
   status = zvunit(&unit2, "INP", 2, NULL);
   status = zvopen(unit2, "OPEN_ACT", "SA", "IO_ACT", "SA",
			"U_FORMAT","HALF", NULL);
   zvget(unit2,"FORMAT",fmt_str, NULL);
   zvget(unit2,"NL",&nl2, NULL);
   zvget(unit2,"NS",&ns2, NULL);
   if ( strcmp(fmt_str,"BYTE") && strcmp(fmt_str,"HALF"))
      zmabend("Invalid input data format.  Use BYTE or HALF.");
   mz_alloc2((unsigned char ***)&buf2,2,ns2,2);
   
   /* set up correlation buffer */
   
   dline = itie[2]-itie[0];
   dsamp = itie[3]-itie[1];
   if (dline<0.0) zmabend("correlation cannot reverse line order");
   dist = sqrt(dline*dline+dsamp*dsamp);
   ncor = (int)(dist+1.0);
   mz_alloc1((unsigned char **)&cx1,ncor,4);
   mz_alloc1((unsigned char **)&cy1,ncor,4);
   mz_alloc1((unsigned char **)&cv1,ncor,4);
   mz_alloc2((unsigned char ***)&cx2,xnum,ncor,4);
   mz_alloc2((unsigned char ***)&cy2,xnum,ncor,4);
   mz_alloc2((unsigned char ***)&cv2,xnum,ncor,4);
   
   for (i=0;i<ncor;i++)
      {
      cx1[i] = itie[0]+i*dline/(float)(ncor-1);
      cy1[i] = itie[1]+i*dsamp/(float)(ncor-1);
      }
   for (i=0;i<ncor;i++)
   for (j=0;j<xnum;j++)
      {
      cx2[j][i] = otie[0]+i*dline/(float)(ncor-1)+(float)j*xdelta[0];
      cy2[j][i] = otie[1]+i*dsamp/(float)(ncor-1)+(float)j*xdelta[1];
      }
   
   /* prime the read buffers */
   
   rline1 = (int)itie[0];
   if (rline1<1||rline1>nl1-1) zmabend("correlation line must be inside image 1");
   ixs = (int)itie[1]-1;
   if (ixs<1||ixs>ns1-1) zmabend("correlation line must be inside image 1");
   status = zvread(unit1,buf1[0],"LINE",rline1, NULL);
   status = zvread(unit1,buf1[1],"LINE",rline1+1, NULL);
   rptr1 = 0;
   rline2 = (int)otie[0];
   if (rline2<1||rline2>nl2-1) zmabend("correlation line must be inside image 2");
   ixs = (int)otie[1]-1;
   if (ixs<1||ixs>ns2-1) zmabend("correlation line must be inside image 2");
   status = zvread(unit2,buf2[0],"LINE",rline2, NULL);
   status = zvread(unit2,buf2[1],"LINE",rline2+1, NULL);
   rptr2 = 0;
   
   /* fill the correlation buffer */
   
   for (i=0;i<ncor;i++)
   for (j=0;j<xnum;j++)
      {
      if (j==0)
         {
         /* do we need a read, file 1 */
         qline = (int)cx1[i];
         while (qline>rline1)
            {
            rline1++;
            status = zvread(unit1,buf1[rptr1],"LINE",rline1+1, NULL);
            rptr1 = 1-rptr1;
            }
         }
      /* do we need a read, file 2 */
      qline = (int)cx2[j][i];
      while (qline>rline2)
         {
         rline2++;
         status = zvread(unit2,buf2[rptr2],"LINE",rline2+1, NULL);
         rptr2 = 1-rptr2;
         }
      if (j==0)
         {/* get value from file 1 */
         fl = cx1[i]-(float)rline1;
         fs = fmod(cy1[i],1.0);
         ixs = (int)cy1[i]-1;
         top = (1.0-fl)*buf1[rptr1][ixs]+fl*buf1[1-rptr1][ixs+1];
         bot = (1.0-fl)*buf1[1-rptr1][ixs]+fl*buf1[rptr1][ixs+1];
         cv1[i] = (1.0-fs)*top+fs*bot;
         }
      /* get value from file 2 */
      fl = cx2[j][i]-(float)rline2;
      fs = fmod(cy2[j][i],1.0);
      ixs = (int)cy2[j][i]-1;
      top = (1.0-fl)*buf2[rptr2][ixs]+fl*buf2[1-rptr2][ixs+1];
      bot = (1.0-fl)*buf2[1-rptr2][ixs]+fl*buf2[rptr2][ixs+1];
      cv2[j][i] = (1.0-fs)*top+fs*bot;
      }
   
   nshftmax = (int)(shiftmax+0.0000001);
   if (apodize)
      {
      sum = 0; pcount = 1;
      for (i=0;i<ncor;i++)
         {
         pval = cv1[i];
         if (pval>zl&&pval<zu) { sum += pval; pcount++; }
         }
      for (i=0;i<ncor;i++)
      for (j=0;j<xnum;j++)
         {
         pval = cv2[j][i];
         if (pval>zl||pval<zu) { sum += pval; pcount++; }
         }
      avg = sum/(float)pcount;
      for (i=0;i<nshftmax*2;i++)
         {
         fs = 0.5*(float)i/shiftmax;
         cv1[i] = fs*cv1[i]+avg;
         cv1[ncor-1-i] = fs*cv1[ncor-1-i]+avg;
         for (j=0;j<xnum;j++)
            {
            cv2[j][i] = fs*cv2[j][i]+avg;
            cv2[j][ncor-1-i] = fs*cv2[j][ncor-1-i]+avg;
            }
         }
      }
   
   /* do the shift calc, integral case */
   
   smax = 1.0e30;
   for (j=0;j<xnum;j++)
      {
      for (ishft=-nshftmax;ishft<=nshftmax;ishft++)
         {
         sum = 0.0; pcount = 1;
         for (i=0;i<ncor;i++)
            {
            pval = cv1[i];
            if (pval<zl||pval>zu) continue;
            pval2 = cv2[j][(i+ishft)%ncor];
            if (pval<zl||pval>zu) continue;
            sum += fabs(pval-pval2);
            pcount++;
            }
         sum /= (float)pcount;
         if (sum<smax)
            {
            imax = ishft;
            jmax = j;
            smax = sum;
            /*printf("j,ishft,pcount,sum %d %d %d %f\n",j,ishft,pcount,sum);*/
            }
         }
      }
   
   /* output shift of file 2 relative to file1 */
   
   mq_out_real("shift",(float)(-imax));
   mq_out_real("xshift",(float)(-jmax));
   mq_out_real("corval",(float)(smax));
   
   return;
}
