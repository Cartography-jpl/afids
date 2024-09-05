#include <math.h>
#include <string.h>
#include <ctype.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

#include "carto/cartoVicarProtos.h"
#include "carto/cartoStrUtils.h"
#include "carto/cartoMemUtils.h"
#include "carto/cartoLsqUtils.h"
#include "carto/cartoGtUtils.h"

/*  GeoTIFF file list routine   A. Zobrist    8/16/99   */

void main44(void)
{
   int i,gtholder,nl,ns,rot,elen,nlen,pcount,pdef,status,len;
   char *labelstr,*p,*printlabel;
   double map[6],voff,vl,vs,gl,gs,east,north;
   double mapunitm,mapinch,scalefrac,horizpix=0;
   double mapinv[6],corner[4];
   
   /* initialize, fetch params */

   zifmessage("gtlist version Wed Jan  2 2008");
   
   status = gtgetlab("inp",1,&labelstr,&nl,&ns);
   if (status!=1)
      zmabend("Failed to read GeoTIFF label");
      
   len = strlen(labelstr);
   if ((printlabel=(char *)malloc(len+1))==NULL) zmabend("malloc failed");
   for (i=0;i<=len;i++)
      {
      printlabel[i] = labelstr[i];
      }
   if (nl==1&&ns==1)
      {
      zvparm("listnl",&nl,&pcount,&pdef,1,0);
      zvparm("listns",&ns,&pcount,&pdef,1,0);
      gtholder = 0;
      }
   else gtholder = 1;
   status = geofix(labelstr,map,mapinv,nl,ns,corner);
   if (status!=1)
      {
      printf("No mapping in GeoTIFF label\n");
      printf("The GeoTIFF label is:\n%s\n\n",printlabel);
      return;
      }
   rot = gtgetrot(labelstr);
      
   zvparmd("mapunitm",&mapunitm,&pcount,&pdef,1,0);
   zvparmd("mapinch",&mapinch,&pcount,&pdef,1,0);
     
   p = ms_find(labelstr,"GTRASTERTYPEGEOKEY=2");
   if (p!=0) voff = 1.0; else voff = 0.5;     /* 0.5 is the default also */
   
   /* printing section */
   
   printf("\n\n      VICAR GeoTIFF LABEL LIST\n\n");
   if (!gtholder)
      {
      printf("The file is a standalone VICAR GeoTIFF label\n");
      printf("A hypothetical %d x %d VICAR image will\n",nl,ns);
      printf("be used to illustrate the mapping of corner points\n\n");
      }
   
   printf("The VICAR GeoTIFF label is:\n%s\n\n",printlabel);
   
   if (voff<0.75) printf("The image raster is an 'area' type\n\n");
      else printf("The image raster is a 'point' or 'post' type\n\n");
   
   printf("The centers of the corner pixels are:\n\n");
   printf("VICAR-line    -samp GeoTIFF-samp    -line");
   printf("            East           North\n\n");
   
   for (i=0;i<4;i++)
      {
      vl = (double)(i/2)*((double)nl-1.0)+1.0;
      vs = (double)(i%2)*((double)ns-1.0)+1.0;
      gl = vl-voff;
      gs = vs-voff;
      east = map[0]*vl+map[1]*vs+map[2];
      elen = MAX(12-(int)(log10(fabs(east)+.9)),1);
      north = map[3]*vl+map[4]*vs+map[5];
      nlen = MAX(12-(int)(log10(fabs(north)+.9)),1);
      printf("%10.1f%9.1f%13.1f%9.1f %15.*f %15.*f\n",
         vl,vs,gs,gl,elen,east,nlen,north);
      }          
   
   printf("\n\nThe outer corners of the corner pixels are:\n\n");
   printf("VICAR-line    -samp GeoTIFF-samp    -line");
   printf("            East           North\n\n");
   
   for (i=0;i<4;i++)
      {
      vl = (double)(i/2)*(double)nl+0.5;
      vs = (double)(i%2)*(double)ns+0.5;
      gl = vl-voff;
      gs = vs-voff;
      east = map[0]*vl+map[1]*vs+map[2];
      elen = MAX(12-(int)(log10(fabs(east)+.9)),1);
      north = map[3]*vl+map[4]*vs+map[5];
      nlen = MAX(12-(int)(log10(fabs(north)+.9)),1);
      printf("%10.1f%9.1f%13.1f%9.1f %15.*f %15.*f\n",
         vl,vs,gs,gl,elen,east,nlen,north);
      }          
   
   printf("\nThe rotation of the image relative to an E-N geographic frame is:\n\n");
   switch (rot)
      {
      case 0:  printf("rotation 0\n369\n258\n147\n\n"); break;
      case 1:  printf("rotation 1\n123\n456\n789\n\n"); break;
      case 2:  printf("rotation 2\n741\n852\n963\n\n"); break;
      case 3:  printf("rotation 3\n987\n654\n321\n\n"); break;
      case 4:  printf("rotation 4\n963\n852\n741\n\n"); break;
      case 5:  printf("rotation 5\n789\n456\n123\n\n"); break;
      case 6:  printf("rotation 6\n147\n258\n369\n\n"); break;
      case 7:  printf("rotation 7\n321\n654\n987\n\n"); break;
      default:
         printf("NOT ALIGNED WITH EAST-NORTH COORDINATE SYSTEM\n\n");
      }
   
   printf("\nThe scale units of the image are (ignoring sign):\n\n");
   switch (rot)
      {
      case 0: case 2: case 4: case 6:
         elen = MAX(13-(int)(log10(fabs(map[4])+.9)),1);
         printf("1 sample = %15.*f map units north\n",elen,fabs(map[4]));
         printf("1 line   = %15.*f map units east\n\n",elen,fabs(map[0]));
         horizpix = (double)nl*map[0];
         break;
      case 1: case 3: case 5: case 7:
         elen = MAX(13-(int)(log10(fabs(map[1])+.9)),1);
         printf("1 sample = %15.*f map units east\n",elen,fabs(map[1]));
         printf("1 line   = %15.*f map units north\n\n",elen,fabs(map[3]));
         horizpix = (double)ns*map[1];
         break;
      default:
         printf("SEE TRANSFORMATION MATRIX\n\n");
      }
   
   scalefrac = fabs((horizpix*mapunitm*39.0)/mapinch);
   printf("\nThe scale fraction is 1 /%9.1f\n",scalefrac);
   printf("(assuming mapunit = %f meters and the map is %f inches)\n\n",
           mapunitm,mapinch);
      
   return;
}
