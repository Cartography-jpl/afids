#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>

#include "defines.h"
#include "carto/cartoGtUtils.h"
#include "carto/cartoMemUtils.h"
#include "carto/cartoStrUtils.h"
#include "carto/cartoLsqUtils.h"
#include "carto/cartoVicarProtos.h"

/*  GeoTIFF ASTERLOG label correction program   P. Kim      */

void getmapping(char *label, double img1[], double img2[], double map[], int index)
{
   char *p;
   double ddummy;

   p = &label[1];
   img1[index] = ms_dnum(&p);
   p++;
   img1[index+3] = ms_dnum(&p);
   p++;
   img1[index+6] = 1.0;
   img2[index] = img1[index];
   img2[index+3] = img1[index+3];
   img2[index+6] = 1.0;

   ddummy = ms_dnum(&p);
   p++;
   map[index] = ms_dnum(&p);
   p++;
   map[index+3] = ms_dnum(&p);
}

void main44(void)
{
   int status;
   int unit;
   int ier, i;
   char mtpg[121][200], translabel[200];
   double img1[9], img2[9], map[6];

   /* initialize, fetch params, two input files a special case */

   zifmessage("gtasterfix version Dec 30 2008");
   
   status = zvunit(&unit, "inp", 1, NULL);
   assert(status == 1);
   status = zvopen(unit, "OP", "UPDATE", NULL);
   assert(status == 1);

   for(i = 0; i < 121; i++)
   {
     status = zlget(unit, "PROPERTY", "MODELTIEPOINTTAG", mtpg[i], "PROPERTY", "GEOTIFF", "ELEMENT", i+1, NULL);
     assert(status == 1);
     //     printf("mtpg: %s\n", mtpg[i]);
   }

   getmapping(mtpg[0], img1, img2, map, 0);
   getmapping(mtpg[10], img1, img2, map, 1);
   getmapping(mtpg[120], img1, img2, map, 2);

   /*
   for(i = 0; i < 9; i++) printf("img1: %f img2: %f\n", img1[i], img2[i]);
   for(i = 0; i < 6; i++) printf("map: %f\n", map[i]);
   */

   dgauss(img1,map,3,1.e-14,&ier);
   if(ier!=0) zmabend("Tiepoints collinear");
   dgauss(img2,&map[3],3,1.e-14,&ier);
   if(ier!=0) zmabend("Tiepoints collinear");

   sprintf(translabel, "(%.13lf,%.13lf,0,%.13lf,%.13lf,%.13lf,0,%.13lf,0,0,0,0,0,0,0,1)", map[0], map[1], map[2], map[3], map[4], map[5]);

   status = zldel(unit, "PROPERTY", "MODELTIEPOINTTAG", "PROPERTY", "GEOTIFF", NULL);
   assert(status == 1);

   status = zladd(unit, "PROPERTY", "GEOGELLIPSOIDGEOKEY", "7030(Ellipse_WGS84)", "PROPERTY", "GEOTIFF", "FORMAT", "STRING", NULL);
   assert(status == 1);
   status = zladd(unit, "PROPERTY", "MODELTRANSFORMATIONTAG", translabel, "FORMAT", "STRING", "PROPERTY", "GEOTIFF", NULL);
   assert(status == 1);

   status = zvclose(unit, NULL);
   assert(status == 1);
}
