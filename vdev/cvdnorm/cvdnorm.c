#include <math.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

#include "carto/cartoVicarProtos.h"
#include "carto/cartoMemUtils.h"

#ifndef MAX
#define MAX(a,b)	(((a)>(b))?(a):(b))
#endif

#ifndef MIN
#define MIN(a,b)	(((a)<(b))?(a):(b))
#endif

/*  cvdnorm   A. Zobrist    01/27/10   */

void main44(void)
{
   int i,j,iline,o_unit[20],nline,nsamp,status,dummy,lnl,lns,pixsiz,nopre;
   int window,win2,don2,jsamp,vunit[20],sigcount,donut,numout,win2m,win2p;
   int iroll,iix,numimg,iw,jw,npair,printls[2],ival,istop,masterband;
   int needctr;
   float sum[20],avg[20],ctr[20],sumdiv,sigma[20],dif[20];
   float background[2][10],rawimg[2][10],pixdif[2][10],rawsig[2][10],
        pixdifadj[2][10],normimg[2][10],mastersigma;
   short int ***inbuf,**outbuf;
   char fmt_str[10];
   zifmessage("cvdnorm version Wed Feb 11 2010");
   
   /* get the parms */
   
   zvp("WINDOW",&window,&dummy);
   if (window%2==0) zmabend("window size must be odd");
   zvp("DONUT",&donut,&dummy);
   if (donut%2==0) zmabend("donut size must be odd");
   win2 = window/2;
   don2 = donut/2;
   status = zvpcnt("inp",&numimg);
   status = zvpcnt("out",&numout);
   if (numout!=numimg) zmabend("number of out images must equal input");
   npair = numimg/2;
   if (npair*2!=numimg) zmabend("number of images must be even");
   zvp("MASTERBAND",&masterband,&dummy);
   zvp("SIGMA",sigma,&sigcount);
   mastersigma = sigma[masterband-1];
   if (sigcount!=numimg) zmabend("wrong number of sigma values");
   zvp("PRINTLS",printls,&dummy);
   win2m=win2-don2;
   win2p=win2+don2;
   nopre = zvptst("nopre");

   /* open the files */
   
   nline = 0; nsamp = 0;
   for (i=0;i<numimg;i++)
      {
      status = zvunit(&vunit[i],"INP",i+1,NULL);
      status = zvopen(vunit[i],"OPEN_ACT","SA","IO_ACT","SA","U_FORMAT","HALF",NULL);
      zvget(vunit[i],"NL",&lnl,"NS",&lns,"PIX_SIZE",&pixsiz,NULL);
      if (i==0) { nline = lnl; nsamp = lns; }
      if (lnl!=nline||lns!=nsamp) zmabend("images must be same size");
      zvget(vunit[i],"FORMAT",fmt_str,NULL);
      if ( strcmp(fmt_str,"BYTE") && strcmp(fmt_str,"HALF"))
         zmabend("Invalid input data format image 1.  Use BYTE or HALF.");
      }
   for (i=0;i<numimg;i++)
      {
      status=zvunit( &o_unit[i],"OUT",i+1,NULL);
      status=zvopen( o_unit[i],"U_NL",nline,"U_NS",nsamp, "U_FORMAT","HALF",
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA",NULL);
      }
   
   mz_alloc3((unsigned char ****)&inbuf,numimg,window,nsamp,2);
   mz_alloc2((unsigned char ***)&outbuf,numimg,nsamp,2);
   
   /* start the barrel */
   
   for (iline=0;iline<=win2;iline++)
      {
      for (i=0;i<numimg;i++) status = zvread(vunit[i],inbuf[i][iline],"LINE",iline+1,
            "SAMP",1,"NSAMPS",nsamp,NULL);
      }
   
   /* read the lines, calculate the dif, write */

   iroll = 0;
   for (iline=0;iline<nline;iline++)
      {
      if (iline<nline-win2)
         {
         for (i=0;i<numimg;i++) status = zvread(vunit[i],inbuf[i][(iline+win2)%window],
            "LINE",iline+win2+1,"SAMP",1,"NSAMPS",nsamp,NULL);
         }
      
      /* calculate */
      for (jsamp=0;jsamp<nsamp;jsamp++)
         {
         for (i=0;i<numimg;i++) sum[i] = 0.0;
         sumdiv = 0.0;
         needctr = 1;
         for (iw=0;iw<window;iw++)
            {
            if (iw<(win2-iline)) continue;
            if (iw>=(nline-iline+win2)) continue;
            iix = (iroll+iw-win2+window)%window;
            for (jw=0;jw<window;jw++)
               {
               if (jw<(win2-jsamp)) continue;
               if (jw>=(nsamp-jsamp+win2)) continue;
               if (needctr)
                  {
                  for (i=0;i<numimg;i++) ctr[i] = (float)inbuf[i][iroll][jsamp];
                  needctr = 0;
                  }
               if (iw<win2m||iw>win2p||jw<win2m||jw>win2p)
                  {
                  istop = 0;
                  for (i=0;i<numimg;i++)
                     {
                     ival = inbuf[i][iix][jsamp+jw-win2];
                     if (ival==0) istop = 1;
                     }
                  if (istop) continue;
                  for (i=0;i<numimg;i++)
                     {
                     ival = inbuf[i][iix][jsamp+jw-win2];
                     sum[i] += ival;
                     }
                  sumdiv += 1.0;
                  }
               }
            }
         
         if (sumdiv<0.5)
            {
            avg[i] = 0.0;
            dif[i] = 0.0;
            }
         else
            {
            for (i=0;i<numimg;i++) avg[i] = sum[i]/sumdiv;
            for (i=0;i<numimg;i++) dif[i] = ctr[i]-avg[i];
            }

         for (i=0;i<2;i++)
            for (j=0;j<npair;j++)
               {
               background[i][j] = avg[i*npair+j];
               rawimg[i][j] = ctr[i*npair+j];
               pixdif[i][j] = dif[i*npair+j];
               }

         /* normalize the pre bands to master band*/

         if (nopre==0) for (i=0;i<npair;i++)
            {
            if (i==masterband-1||ctr[i]==0.0) continue;
            dif[i] *= mastersigma/sigma[i];
            ctr[i] = avg[masterband-1]+dif[i];
            avg[i] = avg[masterband-1];
            }

         /* normalize the post bands to corresp. pre*/
         for (i=0;i<npair;i++)
            {
            if (ctr[i+npair]==0.0) continue;
            dif[i+npair] *= sigma[i]/sigma[i+npair];
            ctr[i+npair] = avg[i]+dif[i+npair];
            }
         
         for (i=0;i<2;i++)
            for (j=0;j<npair;j++)
               {
               rawsig[i][j] = sigma[i*npair+j];
               pixdifadj[i][j] = dif[i*npair+j];
               normimg[i][j] = ctr[i*npair+j];
               }

         if (iline+1==printls[0]&&jsamp+1==printls[1])
            {
            printf("results for image pixel (%d,%d)\n",printls[0],printls[1]);
            printf("sumdiv %f\n",sumdiv);
            zprnt(7,npair,background[0],"background-pre");
            zprnt(7,npair,background[1],"background-post");
            zprnt(7,npair,rawimg[0],"rawimg-pre");
            zprnt(7,npair,rawimg[1],"rawimg-post");
            zprnt(7,npair,pixdif[0],"pixdif-pre");
            zprnt(7,npair,pixdif[1],"pixdif-post");
            zprnt(7,npair,rawsig[0],"rawsig-pre");
            zprnt(7,npair,rawsig[1],"rawsig-post");
            zprnt(7,npair,pixdifadj[0],"pixdifadj-pre");
            zprnt(7,npair,pixdifadj[1],"pixdifadj-post");
            zprnt(7,npair,normimg[0],"normimg-pre");
            zprnt(7,npair,normimg[1],"normimg-post");
            }
         
         for (i=0;i<npair;i++)
            {
            outbuf[i][jsamp] = normimg[0][i];
            outbuf[i+npair][jsamp] = normimg[1][i];
            }
         }
      
      for (i=0;i<npair;i++)
         {
         zvwrit(o_unit[i],outbuf[i],"LINE",iline+1,"SAMP",1,"NSAMPS",nsamp,NULL);
         zvwrit(o_unit[i+npair],outbuf[i+npair],"LINE",iline+1,"SAMP",1,"NSAMPS",nsamp,NULL);
         }
      iroll = (iroll+1)%window;
      }
   
   for (i=0;i<numimg;i++) zvclose(vunit[i],NULL);
   for (i=0;i<numimg;i++) zvclose(o_unit[i],NULL);
   return;
}
