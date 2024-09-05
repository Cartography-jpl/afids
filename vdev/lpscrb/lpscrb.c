/*  linked polygon scribe routine  P.Kim    6/6/07    */
#include "vicmain_c.h"
#include "applic.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "shapefil.h"
#include "LPUtils.h"
#include "MemUtils.h"
#include "mappingUtils.h"
#include "VicHdrs.h"
#include "fileutils.h"

#define wsiz 2000000
#define BYTE_IMG 1
#define HALF_IMG 2
#define FULL_IMG 3

/******************************************************
lpscrb.c

Written by Al Zobrist, Peter Kim.

This function scribes the LP shapes into an
image file.  The algorithm was originally written
by Al Zobrist in polyscrb.c.

8/3/07 - finalized
8/7/07 - modified to handle word images  -pk
******************************************************/

/*******************************************************
This function returns the minimum of the 2 given
integer values.
*******************************************************/
int min(x1, x2)   int x1, x2;
{
   if(x1 > x2) return x2;
   else return x1;
}

/*******************************************************
This function returns the maximum of the 2 given
integer values.
*******************************************************/
int max(x1, x2)
   int x1, x2;
{
   if(x1 > x2) return x1;
   else return x2;
}

/*******************************************************
This function puts the given value into the picture
buffer (void *work) at the given index (i).  The "imgType"
parameter specifies whether the image is byte, full,
half image.
*******************************************************/
void assignWork(work, i, imgType, val)
   void* work;
   int i, imgType, val;
{
   if(imgType == HALF_IMG)
      ((short int*)work)[i] = val;
   else if(imgType == BYTE_IMG)
      ((char *)work)[i] = val;
   else if(imgType == FULL_IMG)
      ((int *)work)[i] = val;
}

/*******************************************************
This function was originally copied and modified from
Al Zobrist's polyscrb.  It draws the LP shape entities
into the image file.
*******************************************************/
void scribe2(list)
   LPList *list;
{
   int status, geounit1, i, j, index;

   /* Al's algorithm
   *****************/

   int ll, lu, ld, oldwtop, wtop, ns4, k, l, imgType, byte, half, full;
   double fxl, fxu, x1, y1, x2, y2, ox2, oy2;
   int ctrev, in_line, isc, jsc, ist, jst, nsqt, lindex, ctspace, xbit, ybit, wsup, rrindex, rrinc, isz, jsz, jstz, istz, zindex;
   void *work;
   double tr[6], tst[4], xd, yd, xdi, ydi, fyl, fyu, tl, tu, t, dx, dy, xt, yt, xq, yq, xinc, yinc, cycle, pmn, pmj, t1, t2;
   int spacing, sl, ss, nl, ns, zoom, tag, dn, bdn, pdn, outside, cnt, def, refl;
   double thresh;
   double sq, px, x, y, py, phi;
   const double degrad = 57.29578;
   zvp("spacing", &spacing, &cnt);
   zvp("sline", &sl, &cnt);
   zvp("ssamp", &ss, &cnt);
   zvp("nline", &nl, &cnt);
   zvp("nsamp", &ns, &cnt);
   zvp("autozoom", &zoom, &cnt);
   zvparmd("nolong", &thresh, &cnt, &def, 0, 0);
   zvp("tag", &tag, &cnt);
   zvp("dn", &dn, &cnt);
   zvp("bdn", &bdn, &cnt);
   zvp("pdn", &pdn, &cnt);
   zvp("outside", &outside, &cnt);
   zvp("refl", &refl, &cnt);
   byte = zvptst("BYTE");
   half = zvptst("HALF");
   full = zvptst("WORD");
   if(half)
   {
      imgType = HALF_IMG;
      mz_alloc1((unsigned char**)&work, wsiz, sizeof(short int));
   }
   else if(byte && !half)
   {
      mz_alloc1((unsigned char**)&work, wsiz, sizeof(char));
      imgType = BYTE_IMG;
   }
   else if(full && !byte && !half)
   {
      mz_alloc1((unsigned char**)&work, wsiz, sizeof(int));
      imgType = FULL_IMG;
   }
   else
      zmabend("Too many image type parameters.\n");
   //   printf("imgType: %d byte: %d half: %d full: %d\n", imgType, byte, half, full);
   zvselpi(0);
   status = zvunit(&geounit1,"out",1,0);
   if(status != 1) IBISSignalU(geounit1, status, 1);

   if(imgType == HALF_IMG)
      status = zvopen(geounit1, "OP", "WRITE", "U_FORMAT", "HALF", "O_FORMAT", "HALF", "U_NL", nl, "U_NS", ns, "OPEN_ACT", "SA", "LAB_ACT", "SA", 0);
   else if(imgType == BYTE_IMG)
      status = zvopen(geounit1, "OP", "WRITE", "U_NL", nl, "U_NS", ns, "OPEN_ACT", "SA", "LAB_ACT", "SA", 0);
   else
      status = zvopen(geounit1, "OP", "WRITE", "U_FORMAT", "FULL", "O_FORMAT", "FULL", "U_NL", nl, "U_NS", ns, "OPEN_ACT", "SA", "LAB_ACT", "SA", 0);


   thresh = thresh*thresh*(nl*nl+ns*ns);
   ns4 = ns+4-ns%4;
   lu = sl; ld = wsiz/ns4;
   wsup = ld*ns4; wtop = wsiz;
   fyl = .4; fyu = ns-.4;

bigloop:
   ll = lu;
   lu += ld-1;if (lu>sl+nl) lu = sl+nl;
   if (ll>=lu) goto fin;
   fxl = ll+.4; fxu = lu+.6;
   oldwtop = wtop;
   wtop = (lu-ll+1)*ns4;
   if (ll==sl) for (i=0;i<wtop;i++) assignWork(work, i, imgType, bdn); //work[i] = bdn;
   if (ll>sl)
   {
      for (i=0;i<ns4;i++)
	 if(imgType == FULL_IMG)
            assignWork(work, i, imgType, ((int*)work)[oldwtop-ns4+i]);
         else if(imgType == HALF_IMG)
            assignWork(work, i, imgType, ((short int*)work)[oldwtop-ns4+i]);//work[i] = work[oldwtop-ns4+i];
	 else if(imgType == BYTE_IMG)
            assignWork(work, i, imgType, ((char *)work)[oldwtop-ns4+i]);
      for (i=ns4;i<wtop;i++) assignWork(work, i, imgType, bdn); //work[i] = bdn;
   }
   for (i=0;i < list->numOfEntities;i++)
   {
      ctspace = 0;
      dn = getTag(list, i);

      for(j = 0; j < (list->vertCnt)[i]; j++)
      {
	 x1 = ox2; y1 = oy2;
         x2 = getDoubleXYorZ(list, i, j, LP_X);
         y2 = getDoubleXYorZ(list, i, j, LP_Y);

	 //if 1st vertex, we can't draw a line (need 2 vertices)
	 if(j==0)
         {
            ox2 = x2;
            oy2 = y2;
            continue;
         }
	 if (refl)
         {
            ox2 = x2; oy2 = y2;
            x2 = tr[0]*ox2+tr[1]*oy2+tr[2];
            y2 = tr[3]*ox2+tr[4]*oy2+tr[5];
         }

	 ox2 = x2; oy2 = y2;
	 if (outside)
	 {
	    px = y1-y2; py = x2-x1;
	    sq = sqrt((double)(px*px+py*py));
	    px = px/sq; py = py/sq;
	    x1 = x1+px; y1 = y1+py;
	    x2 = x2+px; y2 = y2+py;
	 }
	 if (x1<fxl && x2<fxl) continue;
	 if (x1>fxu && x2>fxu) continue;
	 ctrev = 0;
	 if (x1>x2 || (x1==x2 && y1>y2))
	 {
	    x = x1; x1 = x2; x2 = x;
	    y = y1; y1 = y2; y2 = y;
	    ctrev = 1;
	 }
	 xd = x2-x1+1.141593e-9; xdi = 1./xd;
	 yd = y2-y1+1.141593e-9; ydi = 1./yd;
	 tst[0] = (fxl+.1-x1)*xdi;
	 tst[1] = (fyl+.1-y1)*ydi;
	 tst[2] = (fxu-.1-x1)*xdi;
	 tst[3] = (fyu-.1-y1)*ydi;
	 tl = 3.; tu = -3.; in_line = 0;
	 for (k=0;k<4;k++)
	 {
	    x = xd*tst[k]+x1; y = yd*tst[k]+y1;
	    if (x<fxl || x>fxu) continue;
	    if (y<fyl || y>fyu) continue;
	    in_line = in_line || (tst[k]>=0. && tst[k]<=1.);
	    if (tst[k]<tl) tl = tst[k];
	    if (tst[k]>tu) tu = tst[k];
	 }
	 if (tl>2.) continue;
	 if (tl>0.) t = tl; else t = 0.;
	 if (tu>=1.) tu = 1.;
	 if (in_line==0)
	 if (x1<fxl || x1>fxu || y1<fyl || y1>fyu)
            continue;
	 dx = x2-x1; dy = y2-y1;

	 x = xd*t+x1; y = yd*t+y1;
	 isc = max(min((int)x,lu),ll)-ll;
	 jsc = max(min((int)y,ns),0);
	 xt = xd*tu+x1; yt = yd*tu+y1;
	 ist = max(min((int)xt,lu),ll)-ll;
	 jst = max(min((int)yt,ns),0);
	 nsqt = max(abs(isc-ist),abs(jsc-jst));
	 index = isc*ns4+jsc;
	 lindex = ist*ns4+jst;
	 assignWork(work, index, imgType, dn); //work[index] = dn;
	 if (t<.001) assignWork(work, index, imgType, pdn); //work[index] = pdn;
	 if (nsqt==0) continue;
	 xq = isc+ll; yq = jsc;
	 xinc = fabs(xdi);
	 yinc = fabs(ydi);

	 xbit = ns4; ybit = 1;
	 if (xd<0.) xbit = -xbit;
	 if (yd<0.) ybit = -ybit;
	 if (xinc<=yinc)
	 {
	    ybit += xbit; yinc -= xinc;
	    cycle = (xinc+yinc)/xinc;
	    pmn = y-yq; if (yd<0.) pmn = -pmn;
	    pmj = x-xq; if (xd<0.) pmj = -pmj;
	    t1 = xinc*.5;
	    t2 = yinc*(.5-pmn+pmj/cycle);
	 }
	 else
	 {
	    xbit += ybit; xinc -= yinc;
	    cycle = (xinc+yinc)/yinc;
	    pmn = x-xq; if (xd<0.) pmn = -pmn;
	    pmj = y-yq; if (yd<0.) pmj = -pmj;
	    t1 = xinc*(.5-pmn+pmj/cycle);
	    t2 = yinc*.5;
	 }
	 if (ctrev && (ctspace+nsqt)>spacing)
		ctspace = (100*spacing-nsqt-ctspace)%spacing;
	 for (l=0;l<nsqt;l++)
	 {
	    if (t1>=t2)
            {
               index += ybit;
               t2 += yinc;
            }
	    else
            {
               index += xbit;
               t1 += xinc;
            }
	    if (index>wsup) index -= ns4;
	    if (index<0) index += ns4;
	    assignWork(work, index, imgType, dn); //work[index] = dn;
	    ctspace++;
	    if (ctspace==spacing)
	    {
	       ctspace = 0;
	       rrindex = index+rrinc;
	       if (rrindex<=wsup && rrindex>=0)
		  assignWork(work, index, imgType, dn); //work[rrindex] = dn;
	       rrindex = index-rrinc;
	       if (rrindex<=wsup && rrindex>=0)
		  assignWork(work, rrindex, imgType, dn); //work[rrindex] = dn;
	    }
	 }
	 isz = index/ns4; jsz = index-isz*ns4;
	 istz = (ist+isz)/2; jstz = (jst+jsz)/2;
	 zindex = istz*ns4+jstz;
	 assignWork(work, zindex, imgType, dn); //work[zindex] = dn; work[lindex] = dn;
	 assignWork(work, lindex, imgType, dn);
	 if (tu>.999) assignWork(work, lindex, imgType, pdn); //work[lindex] = pdn;
      }
   }
   for (l=ll;l<lu;l++) 
      if(imgType == BYTE_IMG) zvwrit(geounit1, &((char*)work)[(l-ll)*ns4], 0);
      else if(imgType == HALF_IMG) zvwrit(geounit1, &((short int*)work)[(l-ll)*ns4], 0);
      else if(imgType == FULL_IMG) zvwrit(geounit1, &((int*)work)[(l-ll)*ns4], 0);
   goto bigloop;

fin:
   status = zvclose(geounit1, 0);
   if(status != 1) IBISSignalU(geounit1, status, 1);

   destroyLPList(list);
}

/*******************************************************/
void main44()
{
   int ibis;
   int status;
   int i, j, index, geounit1;
   int numOfXYs;
   LPList *list;
   char *lineBuf;
   double minX, maxX, minY, maxY;
   int inpCnt;

   ibis = openIBISInp(1);
   list = slp2LPList(ibis);

   /*
   for(i = 0; i < list->numOfEntities; i++)
      printEntity(list, i);
   */

   status = IBISFileClose(ibis,0);
   if(status!=1) IBISSignal(ibis,status,1);

   zvpcnt("inp", &inpCnt);
   if(inpCnt == 2) lpPixMapConversion(list, 1, 2);
   scribe2(list);
}






