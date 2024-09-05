#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "carto/cartoMemUtils.h"
#include "carto/cartoVicarProtos.h"
#include "carto/cartoStrUtils.h"
#include "carto/cartoGtUtils.h"

static int DEBUG = 0;

/************************************************************************/
/* program rpcinv2                                                      */
/************************************************************************/
/*  3-Dec-07 ...pk... initial version                                   */
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

double getElev(unit, x, y)
   int unit;
   double x, y;
{
   int nl, ns, status;
   int x1, x2, y1, y2;
   double linAbv, linBlw, sampLft, sampRght;
   short *linAbvBuf, *linBlwBuf, f1, f2, f3, f4;
   char fmt[8], errormsg[50];

   zvget(unit, "NL", &nl, "NS", &ns, "FORMAT", fmt, NULL);

   /* get points */
   if(x <= 1)
   {   
      linAbv = 1;
      linBlw = 2;
   }
   else if(x >= nl)
   {
      linAbv = nl-1;
      linBlw = nl;
   }
   else
   {
      linAbv = x;
      linBlw = x+1;
   }

   if(y <= 1)
   {
      sampLft = 1;
      sampRght = 2;
   }
   else if(y >= ns)
   {
      sampLft = ns-1;
      sampRght = ns;
   }
   else
   {
      sampLft = y;
      sampRght = y+1;
   }

   x1=(int)linAbv; y1=(int)sampLft;
   x2=(int)linBlw; y2=(int)sampRght;

   /* read file */
   mz_alloc1((unsigned char**)&linAbvBuf, ns, sizeof(short int));
   mz_alloc1((unsigned char**)&linBlwBuf, ns, sizeof(short int));
   status = zvread(unit, linAbvBuf, "LINE", x1, NULL);
   if(status != 1)
   {
      sprintf(errormsg, "Error while reading above line. Status: %d\n", status);
      zmabend(errormsg);
   }
   status = zvread(unit, linBlwBuf, "LINE", x2, NULL);
   if(status != 1)
   {
      sprintf(errormsg, "Error while reading below line. Status: %d\n", status);
      zmabend(errormsg);
   }
   /* get data */
   f1 = linAbvBuf[y1-1];
   f2 = linAbvBuf[y2-1];
   f3 = linBlwBuf[y1-1];
   f4 = linBlwBuf[y2-1];

   if(DEBUG)
   {
      printf("x: %f y: %f x1: %d x2: %d y1: %d y2: %d\n", x, y, x1, x2, y1, y2);
      printf("p1: %d p2: %d p3: %d p4: %d\n", f1, f2, f3, f4);
   }

   free(linAbvBuf);
   free(linBlwBuf);

   return (f1*(x2-x)*(y2-y)
	   + f2*(x-x1)*(y2-y)
           + f3*(x2-x)*(y-y1)
           + f4*(x-x1)*(y-y1))/((x2-x1)*(y2-y1));
}

void getRpcParams(labelstr, rpck, rpcln, rpcld, rpcsn, rpcsd, rpctype)
   double rpck[], rpcln[], rpcld[], rpcsn[], rpcsd[];
   int *rpctype;
   char *labelstr;
{
   int i, j;
   char *p;

   for (j=0;j<13;j++) rpcrd(0,j,labelstr,rpck);
   for (j=0;j<20;j++) rpcrd(14,j,labelstr,rpcln);
   for (j=0;j<20;j++) rpcrd(15,j,labelstr,rpcld);
   for (j=0;j<20;j++) rpcrd(16,j,labelstr,rpcsn);
   for (j=0;j<20;j++) rpcrd(17,j,labelstr,rpcsd);

   /*
   printf("%s", labelstr);
   printf("rpck\n");
   for (j=0;j<13;j++) printf("%f\n", rpck[j]);
   printf("\nrpcln\n");
   for (j=0;j<20;j++) printf("%f\n", rpcln[j]);
   printf("\nrpcld\n");
   for (j=0;j<20;j++) printf("%f\n", rpcld[j]);
   printf("\nrpcsn\n");
   for (j=0;j<20;j++) printf("%f\n", rpcsn[j]);
   printf("\nrpcsd\n");
   for (j=0;j<20;j++) printf("%f\n", rpcsd[j]);
   printf("\n");
   */

   p = ms_find(labelstr,"NITF_CETAG=");
   if (p!=0)
   {
      if (strncmp(p,"RPC00A",6)==0)
      {
         *rpctype = 0;
         /*printf("processing RPC00A\n");*/
      }
      if (strncmp(p,"RPC00B",6)==0)
      {
         *rpctype = 1;
         /*printf("processing RPC00B\n");*/
      }
   }
   else
   {
      *rpctype = 0;
      /*printf("processing RPC00A by default\n");*/
   }
}

void calculate(demUnit, x, y, lon, lat, elev, tinv, th, rpck, rpcln,
               rpcld, rpcsn, rpcsd, rpctype, fixdted, thresh)
   int demUnit, fixdted;
   double x, y, *lon, *lat, *elev, tinv[], th[], thresh;
   double rpck[], rpcln[], rpcld[], rpcsn[], rpcsd[];
   int rpctype;
{
  int i;
   double oldLon, oldLat, newX, newY;

   if(DEBUG)
   {
      printf("============================\n");
      printf("x: %f y: %f \n", x, y);
   }

   /* initialize values */
   oldLon = oldLat = newX = newY = 0;
   *lon = x*tinv[0] + y*tinv[1] + tinv[2];
   *lat = x*tinv[3] + y*tinv[4] + tinv[5];

   /* calculate */
   for(i = 1; i < 10; i++)
   {
      double demX, demY;

      if(DEBUG)
      {
         printf("iter: %d lon: %0.10f lat: %0.10f elevation: %f\n", i, *lon, *lat, *elev);
         printf("newX: %f newY: %f\n", newX, newY);
      }

      demX = *lon*th[0] + *lat*th[1] + th[2];
      demY = *lon*th[3] + *lat*th[4] + th[5];

      if(!fixdted)
         *elev = getElev(demUnit, demX, demY);

      newX = rpceval(1, *lon, *lat, *elev, rpck, rpcln, rpcld, rpctype) + 0.5;
      newY = rpceval(0, *lon, *lat, *elev, rpck, rpcsn, rpcsd, rpctype) + 0.5;

      *lon -= (newX-x)*tinv[0]+(newY-y)*tinv[1];
      *lat -= (newX-x)*tinv[3]+(newY-y)*tinv[4];

      if(i > 1 && sqrt(pow((*lon-oldLon),2.0)+pow((*lat-oldLat),2.0)) < thresh) break;
      oldLon = *lon; oldLat = *lat;
   }

   if(DEBUG)
   {
      printf("**********\nfinal:\n");
      printf("iter: %d lon: %0.10f lat: %0.10f elevation: %f\n", i, *lon, *lat, *elev);
      printf("newX: %f newY: %f\n", newX, newY);
      printf("\n");
   }
}

int gtgetlab2(geounit, labelstr, nl, ns)
   int *nl, *ns, geounit;
   char **labelstr;
{
   int i, status;
   int maxlen, nelement, len;
   char *buf, valformat[9], vformat[9];
   char svalue[133], key[33];

   mz_alloc1((unsigned char **)&buf, 1000001, 1);

   strcpy(buf, "");
   do
   {
      status = zlninfo(geounit, key, valformat, &maxlen, &nelement, "ERR_ACT", " ", NULL);
      if(status != 1) break;
      if(strcmp(key, "PROPERTY") == 0) continue;
      if(strcmp(key, "NL") == 0)
      {
	status = zlget(geounit, "SYSTEM", key, (char*) nl, "ERR_ACT", "SA", "FORMAT", "INT", NULL);
      }
      if(strcmp(key, "NS") == 0)
      {
	status = zlget(geounit, "SYSTEM", key, (char*) ns, "ERR_ACT", "SA", "FORMAT", "INT", NULL);
      }
      status = zlinfo(geounit, "PROPERTY", key, vformat, &maxlen, &nelement, "ERR_ACT", " ", "PROPERTY", "GEOTIFF", NULL);
      if(status != 1) continue;
      if(strcmp(key, "PROPERTY") == 0) continue;

      for(i = 1; i <= nelement; i++)
      {
 	 if(nelement == 1)
	    status = zlget(geounit, "PROPERTY", key, svalue, "ERR_ACT", "SA", "FORMAT", 
                          "STRING", "NELEMENT", 1, "PROPERTY", "GEOTIFF", "ULEN", 133, NULL);
	 else
	    status = zlget(geounit, "PROPERTY", key, svalue, "ELEMENT", i, "ERR_ACT", "SA",
			   "FORMAT", "STRING", "NELEMENT", 1, "PROPERTY", "GEOTIFF", "ULEN", 133, NULL);
	 strcat(buf, key);
	 strcat(buf, "=");
	 strcat(buf, svalue);
	 strcat(buf, "\n");
      }
   }while(1);

   len = strlen(buf);
   if(((*labelstr) = (char*)malloc(len+1))==NULL) zmabend("malloc failed");
   strcpy(*labelstr, buf);

   free(buf);
   if(strlen(*labelstr)<1) return 0;
   return 1;
}


void getT(maptopix, t, unit, labelstr)
   int maptopix, unit;
   double t[];
   char **labelstr;
{
   int i, ix;
   int status, labnl, labns, len;
   char *buf;
   double tinv[6], corner[4];

   /*calculate the mapping*/
   if(maptopix) ix = 0;
   else ix = 2;

   status = gtgetlab2(unit, &buf, &labnl, &labns);

   len = strlen(buf);
   if(((*labelstr) = (char *)malloc(len+1))==NULL) zmabend("malloc failed");
   strcpy(*labelstr, buf);

   free(buf);

   status = geofix(*labelstr, t, tinv, labnl, labns, corner);
   if(status != 1) zmabend("Failed to get mapping from GeoTIFF label");
   if(ix == 0)
      for(i = 0; i < 6; i++) t[i] = tinv[i];
}

void main44(void)
{
   int i, unit, unit1, unit2, status, ibis, ibisnr;
   double th[6], tinv[6];
   double lon, lat, elev;
   double x, y;
   char *labelstr1, *labelstr2;
   char fixparm[5];
   int fixdted;
   double *lonCol, *latCol, *elevCol, *lineCol, *sampCol, thresh;
   int col[5], colcount, coldef, cnt, def;
   double rpck[13], rpcln[20], rpcld[20], rpcsn[20], rpcsd[20];
   int rpctype;

   zifmessage("rpcinv2 version 19-MAR-09");

   if(!DEBUG) DEBUG = zvptst("print");

   /* open files */
   status = zvunit(&unit, "inp", 1, NULL);
   if(status != 1) zmabend("Error getting ibis file unit.\n");
   status = IBISFileOpen(unit, &ibis, "update", 0, 0, 0, 0);
   if(status != 1) IBISSignalU(unit, status, 1);
   IBISFileGet(ibis, "nr", &ibisnr, 1, 1, 0);
   if(ibisnr == 0) zmabend("Can not process empty IBIS file.\n");
   status = zvunit(&unit1, "inp", 2, NULL);
   if(status != 1) zmabend("Error getting image file unit.\n");
   status = zvopen(unit1, "OP", "READ", "OPEN_ACT", "SA", "LAB_ACT", "SA", NULL);
   if(status != 1) zmabend("Error opening image file.\n");
   status = zvunit(&unit2, "inp", 3, NULL);
   if(status != 1) zmabend("Error getting dem file unit.\n");
   status = zvopen(unit2, "OP", "READ", "OPEN_ACT", "SA", "LAB_ACT", "SA", NULL);
   if(status != 1) zmabend("Error opening image file.\n");
   zvparm("cols", col, &colcount, &coldef, 5, 0);
   if(colcount != 5) zmabend("Requires five columns");
   status = zvparm("fixdted", fixparm, &cnt, &def, 1, 5);
   if(status != 1) zmabend("Error getting fixdted parameter.\n");
   if(strcmp(fixparm,"y") == 0) fixdted = 1;
   else fixdted = 0;
   zvparmd("thresh", &thresh, &cnt, &def, 1, 0);
   if(cnt == 0) thresh = 1.e-10;

   /* initialize data for output */
   mz_alloc1((unsigned char **)&lonCol, ibisnr, sizeof(double));
   mz_alloc1((unsigned char **)&latCol, ibisnr, sizeof(double));
   mz_alloc1((unsigned char **)&elevCol, ibisnr, sizeof(double));
   mz_alloc1((unsigned char **)&lineCol, ibisnr, sizeof(double));
   mz_alloc1((unsigned char **)&sampCol, ibisnr, sizeof(double));

   for(i = 0; i < 5; i++) IBISColumnSet(ibis, "U_FORMAT", "DOUB", col[i]);
   status = IBISColumnRead(ibis, (char*) lineCol, col[3], 1, ibisnr);
   if(status != 1) zmabend("Error getting line data from ibis file.");
   status = IBISColumnRead(ibis, (char*) sampCol, col[4], 1, ibisnr);
   if(status != 1) zmabend("Error getting samp data from ibis file.");
   if(fixdted)
   {
      status = IBISColumnRead(ibis, (char*) elevCol, col[2], 1, ibisnr);
      if(status != 1) zmabend("Error getting elev data from ibis file.");
   }

   /* get mapping info */
   getT(0, tinv, unit1, &labelstr1);
   getT(1, th, unit2, &labelstr2);
   getRpcParams(labelstr1, rpck, rpcln, rpcld, rpcsn, rpcsd, &rpctype);

   for(i = 0; i < ibisnr; i++)
   {
      x = lineCol[i];
      y = sampCol[i];
      if(fixdted) elev=elevCol[i];
      /*
      x = 500000;
      y = 200000;
      */
      calculate(unit2, x, y, &lon, &lat, &elev, tinv, th, rpck, rpcln, rpcld, rpcsn, rpcsd, rpctype, fixdted, thresh);
      lonCol[i] = lon;
      latCol[i] = lat;
      if(!fixdted)elevCol[i] = elev;
   }

   /* close image and dem files */
   zvclose(unit1, NULL);
   zvclose(unit2, NULL);

   if(DEBUG) printf("lon: %0.10f lat: %0.10f elevation: %f\n", lon, lat, elev);

   status = IBISColumnWrite(ibis, (char*) lonCol, col[0], 1, ibisnr);
   status = IBISColumnWrite(ibis, (char*) latCol, col[1], 1, ibisnr);
   status = IBISColumnWrite(ibis, (char*) elevCol, col[2], 1, ibisnr);

   IBISFileClose(ibis, 0);

   free(lonCol);
   free(latCol);
   free(elevCol);
   free(lineCol);
   free(sampCol);
}





