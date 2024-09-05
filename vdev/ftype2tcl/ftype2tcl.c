#include <math.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "vicmain_c.h"
#include "applic.h"
#include "taeconf.inp"
#include "parblk.inc"
#include "defines.h"

#include "geotiffio.h"
#include "geo_tiffp.h"
#include "geo_keyp.h"
#include "geotiff.h"
#include "xtiffio.h"

#include "carto/cartoTaeUtils.h"
#include "carto/cartoVicarProtos.h"

#define TIFFOpen XTIFFOpen
#define TIFFClose XTIFFClose

void main44(void)
{
   int ct, def;			/* for zvparm */
   char infilename[99];
   FILE *infile;
   struct stat statBuf;
   
   zifmessage("ftype2tcl version Wed Jan  2 2008");
   
   /* try to open the file, then write the TAE TCL variable */
   
   zvparm("inp", infilename, &ct, &def, 1, 0);

   if (stat (infilename, & statBuf)) { /* stat failed -- no such file */
     mq_out_int("val", 0);
     mq_out_int("ftype", 0);
     mq_out_int("fsubtype", 0);
     mq_out_int("fsize", 0);
     return;
   }

   mq_out_int("val", 1);

   if (! statBuf.st_size) {	/* empty file */
     mq_out_int("ftype", -2);
     mq_out_int("fsubtype", 0);
     mq_out_int("fsize", 0);
     return;
   }

   mq_out_int("fsize", statBuf.st_size);

   infile = fopen (infilename, "r"); /* open for read failed -- read-protected */
   if (! infile) {
     mq_out_int("ftype", -3);
     mq_out_int("fsubtype", 0);
     mq_out_int("fsize", 0);
     return;
   }

   /* check for TIFF */
   {
     char buf [4];

     if (fread (buf, 1, 4, infile) == 4 && /* see if it looks like a TIFF */
	 ((buf [0] == 0x49 &&
	   buf [1] == 0x49 &&
	   buf [2] == 0x2A &&
	   buf [3] == 0x00) ||
	  (buf [0] == 0x4D &&
	   buf [1] == 0x4D &&
	   buf [2] == 0x00 &&
	   buf [3] == 0x2A)))
       {
	 TIFF *tif;
	 GTIF *gt;
	 int doubleCount;

	 fclose (infile);

	 /* open input TIFF file */
	 if ((tif = TIFFOpen (infilename, "r"))) {
	   pinfo_t *data;

	   if ((gt = GTIFNew (tif)) &&
	       ((gt->gt_methods.get)(tif, GTIFF_GEOKEYDIRECTORY, &gt->gt_nshorts, &data) ||
		(gt->gt_methods.get)(tif, GTIFF_TIEPOINTS, &doubleCount, &data) ||
		(gt->gt_methods.get)(tif, GTIFF_PIXELSCALE, &doubleCount, &data) ||
		(gt->gt_methods.get)(tif, GTIFF_TRANSMATRIX, &doubleCount, &data)))
	     mq_out_int("fsubtype", 2); /* GeoTIFF */
	   else
	     mq_out_int("fsubtype", 3); /* vanilla TIFF */

	   mq_out_int("ftype", 2); /* TIFF */

	   GTIFFree (gt);
	   TIFFClose (tif);
	   return;
	 }
       }
   }

   /* check for VICAR */
   {
     char * lblSizeBuf = "LBLSIZE=";
     char buf [9];

     buf [8] = 0;
     if (! fseek (infile, 0, SEEK_SET) &&
	 fread (buf, 1, 8, infile) == 8 &&
	 ! strcmp (lblSizeBuf, buf)) {

       fclose (infile);

       mq_out_int("ftype", 1);

       {
	 int instances[MAX_PROPS],number_of_props,subset;
	 char prop_names[MAX_PROPS][MAX_LABEL_KEY_SIZE+1];
	 int foundGeotiffLabel = 0;
	 int geounit;

	 zvunit(&geounit,"INP",1, NULL);
	 zvopen(geounit,"OP","READ","OPEN_ACT","SA", "LAB_ACT","SA", NULL);

	 /* Get property names of property subsets */
	 number_of_props = MAX_PROPS;	/* No more than MAX_PROPS allowed */
	 if (zlpinfo(geounit,(char*)prop_names,&number_of_props,
		     "inst_num", instances, "ulen",MAX_LABEL_KEY_SIZE+1, NULL) == 1) {

	   /* Cycle through each subset, listing out all labels */
	   for (subset = 0; subset < number_of_props; subset++)
	     {

	       if (!strcmp ("GEOTIFF", prop_names[subset])) {
		 foundGeotiffLabel = 1;
		 break;
	       }
	     }

	   if (foundGeotiffLabel)
	     mq_out_int("fsubtype", 4);
	   else
	     mq_out_int("fsubtype", 5);
	 } else
	   mq_out_int("fsubtype", 5);

	 zvclose(geounit, NULL);

	 return;
       }
     }
   }

   fclose (infile);

   /* unknown file type */
   mq_out_int("ftype", -1);
   mq_out_int("fsubtype", 0);
   return;
}
