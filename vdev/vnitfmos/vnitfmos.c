#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <assert.h>

void fixLabel(int unit1, int unit2, int unit3)
{
   int status;
   int n1a, n2a, n3a, n4a, e1a, e2a, e3a, e4a;
   int n1b, n2b, n3b, n4b, e1b, e2b, e3b, e4b;
   char lon1[50], lat1[50], lon2[50], lat2[50], lon3[50], lat3[50], lon4[50], lat4[50], igeolo[200], new_igeolo[200];

   status = zlget(unit1, "PROPERTY", "NITF_IGEOLO", igeolo, "PROPERTY", "GEOTIFF", 0);
   assert(status == 1);
   sscanf(igeolo, "%dN%dE%dN%dE%dN%dE%dN%dE", &n1a, &e1a, &n2a, &e2a, &n3a, &e3a, &n4a, &e4a);

   status = zlget(unit2, "PROPERTY", "NITF_IGEOLO", igeolo, "PROPERTY", "GEOTIFF", 0);
   assert(status == 1);
   sscanf(igeolo, "%dN%dE%dN%dE%dN%dE%dN%dE", &n1b, &e1b, &n2b, &e2b, &n3b, &e3b, &n4b, &e4b);
   status = zlget(unit3, "PROPERTY", "NITF_IGEOLO", igeolo, "PROPERTY", "GEOTIFF", 0);
   assert(status == 1);
   status = zlget(unit3, "PROPERTY", "NITF_CORNERLON1", lon1, "PROPERTY", "GEOTIFF", 0);
   assert(status == 1);
   status = zlget(unit3, "PROPERTY", "NITF_CORNERLAT1", lat1, "PROPERTY", "GEOTIFF", 0);
   assert(status == 1);
   status = zlget(unit3, "PROPERTY", "NITF_CORNERLON2", lon2, "PROPERTY", "GEOTIFF", 0);
   assert(status == 1);
   status = zlget(unit3, "PROPERTY", "NITF_CORNERLAT2", lat2, "PROPERTY", "GEOTIFF", 0);
   assert(status == 1);
   status = zlget(unit2, "PROPERTY", "NITF_CORNERLON3", lon3, "PROPERTY", "GEOTIFF", 0);
   assert(status == 1);
   status = zlget(unit2, "PROPERTY", "NITF_CORNERLAT3", lat3, "PROPERTY", "GEOTIFF", 0);
   assert(status == 1);
   status = zlget(unit2, "PROPERTY", "NITF_CORNERLON4", lon4, "PROPERTY", "GEOTIFF", 0);
   assert(status == 1);
   status = zlget(unit2, "PROPERTY", "NITF_CORNERLAT4", lat4, "PROPERTY", "GEOTIFF", 0);
   assert(status == 1);

   sprintf(new_igeolo, "%dN0%dE%dN0%dE%dN0%dE%dN0%dE", n1a, e1a, n2a, e2a, n3b, e3b, n4b, e4b);

   status = zldel(unit3, "PROPERTY", "NITF_IGEOLO", "PROPERTY", "GEOTIFF", 0);
   assert(status == 1);
   status = zladd(unit3, "PROPERTY", "NITF_IGEOLO", new_igeolo, "PROPERTY", "GEOTIFF", "FORMAT", "STRING", 0);
   assert(status == 1);
   status = zldel(unit3, "PROPERTY", "NITF_CORNERLAT3", "PROPERTY", "GEOTIFF", 0);
   assert(status == 1);
   status = zladd(unit3, "PROPERTY", "NITF_CORNERLAT3", lat3, "PROPERTY", "GEOTIFF", "FORMAT", "STRING", 0);
   assert(status == 1);
   status = zldel(unit3, "PROPERTY", "NITF_CORNERLON3", "PROPERTY", "GEOTIFF", 0);
   assert(status == 1);
   status = zladd(unit3, "PROPERTY", "NITF_CORNERLON3", lon3, "PROPERTY", "GEOTIFF", "FORMAT", "STRING", 0);
   assert(status == 1);
   status = zldel(unit3, "PROPERTY", "NITF_CORNERLAT4", "PROPERTY", "GEOTIFF", 0);
   assert(status == 1);
   status = zladd(unit3, "PROPERTY", "NITF_CORNERLAT4", lat4, "PROPERTY", "GEOTIFF", "FORMAT", "STRING", 0);
   assert(status == 1);
   status = zldel(unit3, "PROPERTY", "NITF_CORNERLON4", "PROPERTY", "GEOTIFF", 0);
   assert(status == 1);
   status = zladd(unit3, "PROPERTY", "NITF_CORNERLON4", lon4, "PROPERTY", "GEOTIFF", "FORMAT", "STRING", 0);
   assert(status == 1);

}

void copyImages(int iunit, int ounit, int inoffset, int outoffset, int nl, int ns)
{
   int i, status;
   short int *buf;

   /* initialize data for output */
   buf = (short int *)malloc(ns*sizeof(short int));

   for(i = inoffset; i < nl; i++)
   {
      status = zvread(iunit, buf, "LINE", i+1, 0);
      assert(status == 1);
      status = zvwrit(ounit, buf, "LINE", ++outoffset, 0);
      assert(status == 1);
   }

   free(buf);
}

void main44(void)
{
   int unit1, unit2, unit3, status, cnt;
   int nl1, ns1, nl2, ns2;
   int ioff, tnl;
   char fmt[5];

   status = zvp("ioff", &ioff, &cnt);
   if(status != 1) zabend();

   if(ioff < 0) zmabend("!!!!!ioff value must not be negative!!!!!\n");

   /* open input files */
   status = zvunit(&unit1, "inp", 1, 0);
   if(status != 1) zabend();
   status = zvopen(unit1, "OP", "READ", 0);
   if(status != 1) zabend();
   status = zvget(unit1, "NL", &nl1, "NS", &ns1, "FORMAT", fmt, 0);
   if(status != 1) zabend();

   status = zvunit(&unit2, "inp", 2, 0);
   if(status != 1) zabend();
   status = zvopen(unit2, "OP", "READ", 0);
   if(status != 1) zabend();
   status = zvget(unit2, "NL", &nl2, "NS", &ns2, 0);
   if(status != 1) zabend();

   if(ns1 != ns2) zmabend("!!!!!the number of samples in the 2 input images do not match!!!!!!\n");

   tnl = nl2+ioff;

   status = zvunit(&unit3, "out", 1, 0);
   if(status != 1) zabend();
   status = zvopen(unit3, "OP", "WRITE", "U_NL", tnl, "U_NS", ns1, "O_FORMAT", fmt, "U_FORMAT", fmt,
                   "OPEN_ACT", "SA", "LAB_ACT", "SA", 0);
   if(status != 1) zabend();

   copyImages(unit1, unit3, 0, 0, nl1, ns1);
   copyImages(unit2, unit3, nl1 - ioff, nl1, nl2, ns2);

   fixLabel(unit1, unit2, unit3);

   /* close image and dem files */
   zvclose(unit1, 0);
   zvclose(unit2, 0);
   zvclose(unit3, 0);
}
