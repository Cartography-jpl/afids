#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "datum.h"
#include "tranmerc.h"
#include "mercator.h"
#include "lambert.h"
#include "ups.h"
#include "eqdcyl.h"
#include "cassini.h"
#include "miller.h"
#include "orthogr.h"
#include "polycon.h"
#include "sinusoid.h"
#include "grinten.h"
#include "utm.h"
#include "albers.h"
#include "ellipse.h"

#include "carto/cartoVicarProtos.h"
#include "carto/cartoStrUtils.h"
#include "carto/cartoMemUtils.h"
#include "carto/cartoGtUtils.h"

#define NUMKEYS 18
#define NUMVALS 78
#define NUMPCS 6

#define NUMTECELLIPS 23
#define NUMCROSSMAP 29

/************************************************************************/
/* program gtproj                                                      */
/************************************************************************/
/*  00-02 ...alz... initial version                     */
/************************************************************************/


/* <<<< Cartographic projection filter program >>>> */

#define MAX_LINE 200
#define MAX_PARGS 100

#define RAD_TO_DEG      57.29577951308232
#define DEG_TO_RAD      .0174532925199432958

char transarg[50][51],ep_par[151];

double **xyzin,**xyzout;
long NIMAerrcode,NIMAcount;
int clen;

/*===================================================================

NIMAdatumSub

NIMAdatumSub converts datum using NIMA subroutines

function return : void

arguments :

      1. indatum:   input, int indatum;
         index in 3_param.dat of the input NIMA datum
      2. outdatum:  output, int outdatum;
         index in 3_param.dat of the output NIMA datum
      
*/
 
void NIMAdatumSub(indatum,outdatum)
   int indatum,outdatum;
{
   int i;
   double x,y,z,pi,degrad,raddeg;
   
   pi = 3.14159265358979323;
   raddeg = pi/180.0;
   degrad = 1.0/raddeg;
   
   for (i=0;i<clen;i++)
      {
      xyzin[0][i] = xyzin[0][i]*raddeg;
      xyzin[1][i] = xyzin[1][i]*raddeg;
      if (indatum==(-1)&&outdatum==0)
         {
         Geodetic_Shift_WGS72_To_WGS84(xyzin[1][i],xyzin[0][i],xyzin[2][i],
             &xyzout[1][i],&xyzout[0][i],&xyzout[2][i]);
         }
      else if (indatum==0&&outdatum==(-1))
         {
         Geodetic_Shift_WGS84_To_WGS72(xyzin[1][i],xyzin[0][i],xyzin[2][i],
             &xyzout[1][i],&xyzout[0][i],&xyzout[2][i]);
         }
      else if (indatum>0&&outdatum==0)
         {
         Geodetic_Shift_To_WGS84(indatum,xyzin[1][i],xyzin[0][i],xyzin[2][i],
             &xyzout[1][i],&xyzout[0][i],&xyzout[2][i]);
         }
      else if (indatum==0&&outdatum>0)
         Geodetic_Shift_From_WGS84(xyzin[1][i],xyzin[0][i],xyzin[2][i],outdatum,
             &xyzout[1][i],&xyzout[0][i],&xyzout[2][i]);
      else if (indatum>0&&outdatum==(-1))
         {
         Geodetic_Shift_To_WGS84(indatum,xyzin[1][i],xyzin[0][i],xyzin[2][i],&y,&x,&z);
         Geodetic_Shift_WGS84_To_WGS72(y,x,z,&xyzout[1][i],&xyzout[0][i],&xyzout[2][i]);
         }
      else if (indatum==(-1)&&outdatum>0)
         {
         Geodetic_Shift_WGS72_To_WGS84(xyzin[1][i],xyzin[0][i],xyzin[2][i],&y,&x,&z);
         Geodetic_Shift_From_WGS84(y,x,z,outdatum,&xyzout[1][i],&xyzout[0][i],&xyzout[2][i]);
         }
      else if (indatum>0&&outdatum>0)
         {
         Geodetic_Shift_To_WGS84(indatum,xyzin[1][i],xyzin[0][i],xyzin[2][i],&y,&x,&z);
         Geodetic_Shift_From_WGS84(y,x,z,outdatum,&xyzout[1][i],&xyzout[0][i],&xyzout[2][i]);
         }
      else zmabend("logic failure in NIMA datum codes");
      xyzout[0][i] = xyzout[0][i]*degrad;
      xyzout[1][i] = xyzout[1][i]*degrad;
      xyzin[0][i] = xyzout[0][i];
      xyzin[1][i] = xyzout[1][i];
      }
   return;
}


/*===================================================================

GeotiffToEposc

GeotiffToEposc converts a GeoTIFF label (in a string) to a set of
strings **argv and a count argc suitable for EPOSC mapping routines

function return : void

arguments :

      1. labelstr: input, char *labelstr;
         The GeoTIFF label returned by subroutine gtgetlab()
      2. pargc:    output, int *pargc;
         The count of EPOSC parameters
      3. pargv:    output, char **pargc;
         The pointers to EPOSC parameters, each a string
      4. domapp:   output, int *domapp;
         The count of EPOSC parameters
      5. dir:      input, int dir;
         1 = inverse, 0 = forward
      6. datum:    output, int *datum;
         1 = inverse, 0 = forward
      
*/
 
void GeotiffToEposc(labelstr,pargc,pargv,domapp,dir,datum)
   int *pargc,*domapp,dir,*datum;
   char *labelstr,**pargv;
{ 
   int i,j,gtsave=0,tid,tval,notfound,ptr,secondcopy;
   long dtemp;
   char *p,*q,*psav,temp[20];
   struct KEYTRAN
      {
      int id;
      char *gtkey;
      char *epkey;
      };
   struct VALTRAN
      {
      int id;
      int gtval;
      char *epval;
      };
   struct PCSTRAN
      {
      int id;
      int gtminval;
      int gtmaxval;
      char *gtpcs;    /* not used in code, only to help reading */
      char *epproj;
      char *epellps;  /* this is now the NIMA 2 letter code in ellips.dat */
      int bzone;
      int offsetzone;
      int bsouth;
      char *datumcode;
      };
   struct PCSTRAN ptran[NUMPCS] = 
     {{0,32201,32260,"PCS_WGS72_UTM_ZONE_1N","utm","WD",1,32200,0,"WGS72"},
      {1,32301,32360,"PCS_WGS72_UTM_ZONE_1S","utm","WD",1,32300,1,"WGS72"},
      {2,32601,32660,"PCS_WGS84_UTM_ZONE_1N","utm","WE",1,32600,0,"WGS84"},
      {3,32701,32760,"PCS_WGS84_UTM_ZONE_1S","utm","WE",1,32700,1,"WGS84"},
      {4,26903,26923,"PCS_NAD83_UTM_ZONE_1N","utm","RF",1,26900,0,"NAR-C"},
      {4,26703,26922,"PCS_NAD27_UTM_ZONE_1N","utm","CC",1,26700,0,"NAS-C"}};
   struct KEYTRAN ktran[NUMKEYS] = 
     {{0,"PROJECTIONGEOKEY=","+proj="},
      {1,"GEOGELLIPSOIDGEOKEY=","+ellps="},
      {2,"GEOGANGULARUNITSGEOKEY=","+xxx="},
      {3,"PROJECTEDCSTYPEGEOKEY=",""},
      {4,"GTMODELTYPEGEOKEY=",""},
      {5,"GEOGSEMIMAJORAXISGEOKEY=","+a="},
      {6,"GEOGSEMIMINORAXISGEOKEY=","+b="},
      {7,"GEOGINVFLATTENINGGEOKEY=","+f="},
      {8,"PROJLINEARUNITSGEOKEY=","+units="},
      {9,"PROJSTDPARALLEL1GEOKEY=","+lat_1="},
      {10,"PROJSTDPARALLEL2GEOKEY=","+lat_2="},
      {11,"PROJFALSEEASTINGGEOKEY=","+x_0="},
      {12,"PROJFALSENORTHINGGEOKEY=","+y_0="},
      {13,"PROJCENTERLONGGEOKEY=","+lon_0="},
      {14,"PROJCENTERLATGEOKEY=","+lat_0="},
      {15,"PROJAZIMUTHANGLEGEOKEY=","+alpha="},
      {16,"PROJSTRAIGHTVERTICALPOLELONGGEOKEY=","+rlon_0="},
      {17,"GEOGGEODETICDATUMGEOKEY=","+geodeticdatum="}};
   /* long names below are GeoTIFF names that are not translated to eposc */
   /* fourth parameter is for names that imply a NIMA datum */
   struct VALTRAN vtran[NUMVALS] = 
     {{0,0,"geographic"},
      {0,1,"tmerc"},
      {0,2,"transverse_mercator_modified_alaska"},
      {0,3,"omerc"},
      {0,4,"labrd"},
      {0,5,"oblique_mercator_rosenmund"},
      {0,6,"oblique_mercator_spherical"},
      {0,7,"merc"},
      {0,8,"lcc"},
      {0,9,"lambert_conformal_conic_helmert"},
      {0,10,"laea"},
      {0,11,"aea"},
      {0,12,"aeqd"},
      {0,13,"eqdc"},
      {0,14,"stere"},
      {0,15,"ups"},
      {0,16,"oblique_stereographic"},
      {0,17,"eqc"},
      {0,18,"cass"},
      {0,19,"gnom"},
      {0,20,"mill"},
      {0,21,"ortho"},
      {0,22,"poly"},
      {0,23,"robin"},
      {0,24,"sinu"},
      {0,25,"vandg"},
      {0,26,"nzmg"},
      {0,27,"tmerc"},
      {1,7001,"airy"},
      {1,7002,"mod_airy"},
      {1,7003,"aust_SA"},
      {1,7004,"bessel"},
      {1,7005,"ellipse_bessel_modified"},
      {1,7006,"bess_nam"},
      {1,7007,"ellipse_clarke_1858    "},
      {1,7008,"clrk66"},
      {1,7009,"ellipse_clarke_1866_michigan"},
      {1,7010,"ellipse_clarke_1880_benoit"},
      {1,7011,"ellipse_clarke_1880_IGN"},
      {1,7012,"ellipse_clarke_1880_RGS"},
      {1,7013,"ellipse_clarke_1880_arc"},
      {1,7014,"ellipse_clarke_1880_SGA_1922"},
      {1,7015,"ellipse_everest_1830_1937_adjustment"},
      {1,7016,"ellipse_everest_1830_1967_adjustment"},
      {1,7017,"ellipse_everest_1830_1975_adjustment"},
      {1,7018,"ellipse_everest_1830_modified"},
      {1,7019,"GRS80"},
      {1,7020,"helmert"},
      {1,7021,"clrk66"},
      {1,7022,"intl"},
      {1,7023,"new_intl"},
      {1,7024,"krass"},
      {1,7025,"NWL9D"},
      {1,7026,"ellipse_NWL10D         "},
      {1,7027,"plessis"},
      {1,7028,"ellipse_struve_1860    "},
      {1,7029,"ellipse_war_office     "},
      {1,7030,"WGS84"},
      {1,7031,"ellipse_GEM_10C        "},
      {1,7032,"ellipse_OSU86F         "},
      {1,7033,"ellipse_OSU91A         "},
      {1,7034,"clrk80"},
      {1,7035,"ellipse_sphere         "},
      {8,9001,"m"},
      {8,9002,"ft"},
      {8,9003,"us-ft"},
      {8,9004,"linear_foot_modified_american"},
      {8,9005,"linear_foot_clarke     "},
      {8,9006,"ind-ft"},
      {8,9007,"link"},
      {8,9008,"linear_link_benoit     "},
      {8,9009,"linear_link_sears      "},
      {8,9010,"linear_chain_benoit    "},
      {8,9011,"linear_chain_sears     "},
      {8,9012,"linear_yard_sears     "},
      {8,9013,"ind-yd"},
      {8,9014,"fath"},
      {8,9015,"kmi"}};
   
   *datum = 0; /*  the default is WGS84 = 0 */
   
   *domapp = 1;
   if (dir) strcpy(transarg[0],"inv");
   else strcpy(transarg[0],"fwd");
   *pargc = 1;
   
   for (i=0;i<NUMKEYS;i++)
      {
      p = ms_find(labelstr,ktran[i].gtkey);
      if (p!=NULL)
         {
         if (i==3) /* PCS case */
            {
            tval = ms_num(p);
            for (j=0;j<NUMPCS;j++)
               {
               if (tval>=ptran[j].gtminval&&tval<=ptran[j].gtmaxval)
                  {
                  strcpy(transarg[*pargc],"+proj=");
                  strcat(transarg[*pargc],ptran[j].epproj);
                  (*pargc)++;
                  strcpy(transarg[*pargc],"+ellps=");
                  strcat(transarg[*pargc],ptran[j].epellps);
                  (*pargc)++;
                  if (ptran[j].bzone)
                     {
                     strcpy(transarg[*pargc],"+zone=");
                     sprintf(temp,"%d",tval-ptran[j].offsetzone);
                     strcat(transarg[*pargc],temp);
                     (*pargc)++;
                     }
                  if (ptran[j].bsouth)
                     {
                     strcpy(transarg[*pargc],"+south");
                     (*pargc)++;
                     }
                  /*lookup NIMA datum*/
                  
                  /*?? ptran[j].datumcode is the string
                  ?? *datum is the output*/
                  
                  if (strcmp(ptran[j].datumcode,"WGS72")==0) *datum = -1;
                  else if (strcmp(ptran[j].datumcode,"WGS84")==0) *datum = 0;
                  else
                     {
                     NIMAerrcode = Datum_Index(ptran[j].datumcode,&dtemp);
                     if (NIMAerrcode!=0) zmabend("error finding NIMA datum code");
                     *datum = (int)dtemp;
                     }
                  
                  strcpy(transarg[*pargc],"+datum=");
                  strcat(transarg[*pargc],ptran[j].datumcode);
                  (*pargc)++;
                  break;
                  }
               }
            continue;
            }
         if (i==17) /* nima datum case */
            {
            tval = ms_num(p);
            if (tval==39991)
               {
               strcpy(transarg[*pargc],"+datum=WGS72");
               (*pargc)++;
               *datum = -1;
               continue;
               }
            if (tval==39992)
               {
               strcpy(transarg[*pargc],"+datum=WGS84");
               (*pargc)++;
               *datum = 0;
               continue;
               }
            if (tval==39990)
               {
               for (j=0;j<5;j++) temp[j] = *(p+j+11);
               temp[5] = (char)0;
               strcpy(transarg[*pargc],"+datum=");
               strcat(transarg[*pargc],temp);
               (*pargc)++;
               NIMAerrcode = Datum_Index(temp,&dtemp);
               if (NIMAerrcode!=0) zmabend("error finding NIMA datum code");
               *datum = (int)dtemp;
               continue;
               }
            if (tval==6269) continue; /* assume PCS code in label */
            zmabend("bad code for GEOGGEODETICDATUMGEOKEY");
            continue;
            }
         if (i==4) /* geographic coord case */
            {
            if (*p=='2') *domapp = 0;
            continue;
            }
         strcpy(transarg[*pargc],ktran[i].epkey);
         tid = ktran[i].id;
         tval = ms_num(p);
         notfound = 1;
         for (j=0;j<NUMVALS;j++)
            {
            if (vtran[j].id==tid&&vtran[j].gtval==tval)
               {
               strcat(transarg[*pargc],vtran[j].epval);
               notfound = 0;
               gtsave = tval;
               if (strlen(vtran[j].epval)>20)
                  {
                  printf("GeoTIFF: %s\n",vtran[j].epval);
                  zmabend("GeoTIFF term not mapped to eposc term");
                  }
               continue;
               }
            }
         psav = p;
         if (notfound)
            {
            q = strpbrk(p,"(,\n");
            for (ptr=strlen(transarg[*pargc]);p<q;p++,ptr++)
               {
               transarg[*pargc][ptr] = *p;
               transarg[*pargc][ptr+1] = (char)0;
               }
            }
         (*pargc)++;
         p = psav;
         secondcopy = 0;
         if (i==0&&gtsave==27)
            {
            strcpy(transarg[*pargc],"+south");
            (*pargc)++;
            }
         if (i==14)
            {
            strcpy(transarg[*pargc],"+lat_ts=");
            secondcopy = 1;
            }
         if (i==15)
            {
            strcpy(transarg[*pargc],"+azi=");
            secondcopy = 1;
            }
         if (secondcopy)
            {
            q = strpbrk(p,"(,\n");
            for (ptr=strlen(transarg[*pargc]);p<q;p++,ptr++)
               {
               transarg[*pargc][ptr] = *p;
               transarg[*pargc][ptr+1] = (char)0;
               }
            (*pargc)++;
            }
         }
      }
   
   if (dir)
      {
      if (*domapp) printf("\ninverse applied\n");
      else printf("\ninverse not applied\n");
      }
   else
      {
      if (*domapp) printf("\nforward applied\n");
      else printf("\nforward not applied\n");
      }
   if (strlen(ep_par)>0) strcpy(transarg[(*pargc)++],ep_par);
   for (i=0;i<*pargc;i++)
     {
     pargv[i] = transarg[i];
     if (*domapp) printf("eposc arg %2d: %s\n",i+1,transarg[i]);
     }
}

int getargint(argc,argv,keystring,defaultreturn)
   int argc,defaultreturn;
   char **argv,*keystring;
{
   int i;
   char *p;
   
   for (i=0;i<argc;i++)
      {
      p = ms_find(argv[i],keystring);
      if (p!=0) return(ms_num(p));
      }
   return (defaultreturn);
}

int getargboolean(argc,argv,keystring)
   int argc;
   char **argv,*keystring;
{
   int i;
   char *p;
   
   for (i=0;i<argc;i++)
      {
      p = ms_find(argv[i],keystring);
      if (p!=0) return(1);
      }
   return(0);
}

double getargdouble(argc,argv,keystring,defaultreturn)
   int argc;
   double defaultreturn;
   char **argv,*keystring;
{
   int i;
   char *p;
   
   for (i=0;i<argc;i++)
      {
      p = ms_find(argv[i],keystring);
      if (p!=0) return(ms_dnum(&p));
      }
   return (defaultreturn);
}

void getargstring(argc,argv,keystring,retstring)
   int argc;
   char **argv,*keystring,*retstring;
{
   int i;
   char *p;
   
   for (i=0;i<argc;i++)
      {
      p = ms_find(argv[i],keystring);
      if (p!=0)
         {
         strcpy(retstring,p);
         return;
         }
      }
   strcpy(retstring,"");
   return;
}

void geotranssub(argc,argv)
   int argc;
   char **argv;
{
   int i,j,nproj=0,nellipse,geotransinverse;
   long utmzone,utmzoneoverride,ierror=0;
   double proja,projb,projf,u1,v1,u2,v2,dummyscale;
   char tempstring[20],hemisphere,hemisphereoverride;
   struct ELPSTRAN
      {
      int id;
      char *gtellips;
      char *tecellips;
      double aellipse;
      double bellipse;
      double fellipse;
      };
   struct ELPSTRAN etran[NUMTECELLIPS] = 
     {{0,"airy","AA",6377563.396,6356256.9090,299.324964600},
      {1,"mod_airy","AM",6377340.189,6356034.4480,299.324964600},
      {2,"aust_SA","AN",6378160.000,6356774.7190,298.250000000},
      {3,"bessel","BR",6377397.155,6356078.9630,299.152812800},
      {4,"bess_nam","BN",6377483.865,6356165.3830,299.152812800},
      {5,"clrk66","CC",6378206.400,6356583.8000,294.978698200},
      {6,"clrk80","CD",6378249.145,6356514.8700,293.465000000},
      {7,"evrst30","EA",6377276.345,6356075.4130,300.801700000},
      {8,"evrstSS","EB",6377298.556,6356097.5500,300.801700000},
      {9,"evrst56","EC",6377301.243,6356100.2280,300.801700000},
      {10,"evrst69","ED",6377295.664,6356094.6680,300.801700000},
      {11,"evrst48","EE",6377304.063,6356103.0390,300.801700000},
      {12,"xxxxx","EF",6377309.613,6356109.5710,300.801700000},
      {13,"fschr60m","FA",6378155.000,6356773.3200,298.300000000},
      {14,"GRS80","RF",6378137.000,6356752.3141,298.257222101},
      {15,"helmert","HE",6378200.000,6356818.1700,298.300000000},
      {16,"hough","HO",6378270.000,6356794.3430,297.000000000},
      {17,"GRS67","ID",6378160.000,6356774.5040,298.247000000},
      {18,"intl","IN",6378388.000,6356911.9460,297.000000000},
      {19,"krass","KA",6378245.000,6356863.0190,298.300000000},
      {20,"aust_SA","SA",6378160.000,6356774.7190,298.250000000},
      {21,"WGS72","WD",6378135.000,6356750.5200,298.260000000},
      {22,"WGS84","WE",6378137.000,6356752.3142,298.257223563}};
   struct VALTRAN
      {
      int id;
      int gtval;
      char *epval;
      };
    struct VALTRAN vtran[NUMCROSSMAP] = 
     {{0,0,"geographic"},
      {0,1,"tmerc"},
      {0,2,"transverse_mercator_modified_alaska"},
      {0,3,"omerc"},
      {0,4,"labrd"},
      {0,5,"oblique_mercator_rosenmund"},
      {0,6,"oblique_mercator_spherical"},
      {0,7,"merc"},
      {0,8,"lcc"},
      {0,9,"lambert_conformal_conic_helmert"},
      {0,10,"laea"},
      {0,11,"aea"},
      {0,12,"aeqd"},
      {0,13,"eqdc"},
      {0,14,"stere"},
      {0,15,"ups"},
      {0,16,"oblique_stereographic"},
      {0,17,"eqc"},
      {0,18,"cass"},
      {0,19,"gnom"},
      {0,20,"mill"},
      {0,21,"ortho"},
      {0,22,"poly"},
      {0,23,"robin"},
      {0,24,"sinu"},
      {0,25,"vandg"},
      {0,26,"nzmg"},
      {0,27,"tmerc"},
      {0,28,"utm"}};
      
   /* dummyscale is returned by Set routines, but not used?????? */
   
   /* what is this? */
   
   long Set_Albers_Parameters(); /* do I need to get all of these */
   
   /* determine inverse */
   
   geotransinverse = getargboolean(argc,argv,"inv");
   
   /* get the ellipse code */
   
   getargstring(argc,argv,"+ellps=",tempstring);
   
   /* old code, should use this */
   
   /*nellipse = 22;
   for (i=0;i<NUMTECELLIPS;i++)
      if (strcmp(etran[i].gtellips,tempstring)==0) nellipse = i;
   Ellipsoid_Index(etran[nellipse].tecellips,&tecellipseindex);
   Ellipsoid_Axes(tecellipseindex,&proja,&projb);
   projf = (proja-projb)/proja;*/
   
   /* SUBSTITUTE CODE FOR ELLIPSE */
   
   getargstring(argc,argv,"+ellps=",tempstring);
   nellipse = 22;
   for (i=0;i<NUMTECELLIPS;i++)
      if (strcmp(etran[i].tecellips,tempstring)==0) nellipse = i;
   proja = etran[nellipse].aellipse;
   projb = etran[nellipse].bellipse;
   projf = (proja-projb)/proja;
   /* get projection id */
   
   getargstring(argc,argv,"+proj=",tempstring);
   for (i=0;i<NUMCROSSMAP;i++)
      if (strcmp(vtran[i].epval,tempstring)==0) nproj = vtran[i].gtval;
   if (nproj<0) zmabend("Projection not found in GeoTIFF label");
   
   /* special cases */
   
   utmzoneoverride = (long)getargdouble(argc,argv,"+zone=",0);
   if (getargboolean(argc,argv,"+south"))
      hemisphereoverride = 'S';
   else
      hemisphereoverride = 'N';
   
   /* initialize the projection */
   
   switch(nproj)
      {
      case  1:
      case 27: ierror = Set_Transverse_Mercator_Parameters(proja,projf,
                  getargdouble(argc,argv,"+lat_0=",(double)0.0)*DEG_TO_RAD,
                  getargdouble(argc,argv,"+lon_0=",(double)0.0)*DEG_TO_RAD,
                  getargdouble(argc,argv,"+x_0=",(double)0.0),
                  getargdouble(argc,argv,"+y_0=",(double)0.0),
                  getargdouble(argc,argv,"+scale=",(double)1.0));
               break;
      case  7: ierror = Set_Mercator_Parameters(proja,projf,
                  getargdouble(argc,argv,"+lat_0=",(double)0.0)*DEG_TO_RAD,
                  getargdouble(argc,argv,"+lon_0=",(double)0.0)*DEG_TO_RAD,
                  getargdouble(argc,argv,"+x_0=",(double)0.0),
                  getargdouble(argc,argv,"+y_0=",(double)0.0),
                  &dummyscale);
               break;
      case  8: ierror = Set_Lambert_Parameters(proja,projf,
                  getargdouble(argc,argv,"+lat_0=",(double)0.0)*DEG_TO_RAD,
                  getargdouble(argc,argv,"+lon_0=",(double)0.0)*DEG_TO_RAD,
                  getargdouble(argc,argv,"+lat_1=",(double)0.0)*DEG_TO_RAD,
                  getargdouble(argc,argv,"+lat_2=",(double)0.0)*DEG_TO_RAD,
                  getargdouble(argc,argv,"+x_0=",(double)0.0),
                  getargdouble(argc,argv,"+y_0=",(double)0.0));
               break;
      case 11: ierror = Set_Albers_Parameters(proja,projf,
                  getargdouble(argc,argv,"+lat_0=",(double)0.0)*DEG_TO_RAD,
                  getargdouble(argc,argv,"+lon_0=",(double)0.0)*DEG_TO_RAD,
                  getargdouble(argc,argv,"+lat_1=",(double)0.0)*DEG_TO_RAD,
                  getargdouble(argc,argv,"+lat_2=",(double)0.0)*DEG_TO_RAD,
                  getargdouble(argc,argv,"+x_0=",(double)0.0),
                  getargdouble(argc,argv,"+y_0=",(double)0.0));
               break;
      case 15: ierror = Set_UPS_Parameters(proja,projf);
               break;
      case 17: ierror = Set_Equidistant_Cyl_Parameters(proja,projf,
                  getargdouble(argc,argv,"+lat_0=",(double)0.0)*DEG_TO_RAD,
                  getargdouble(argc,argv,"+lon_0=",(double)0.0)*DEG_TO_RAD,
                  getargdouble(argc,argv,"+x_0=",(double)0.0),
                  getargdouble(argc,argv,"+y_0=",(double)0.0));
               break;
      case 18: ierror = Set_Cassini_Parameters(proja,projf,
                  getargdouble(argc,argv,"+lat_0=",(double)0.0)*DEG_TO_RAD,
                  getargdouble(argc,argv,"+lon_0=",(double)0.0)*DEG_TO_RAD,
                  getargdouble(argc,argv,"+x_0=",(double)0.0),
                  getargdouble(argc,argv,"+y_0=",(double)0.0));
               break;
      case 20: ierror = Set_Miller_Parameters(proja,projf,
                  getargdouble(argc,argv,"+lon_0=",(double)0.0)*DEG_TO_RAD,
                  getargdouble(argc,argv,"+x_0=",(double)0.0),
                  getargdouble(argc,argv,"+y_0=",(double)0.0));
               break;
      case 21: ierror = Set_Orthographic_Parameters(proja,projf,
                  getargdouble(argc,argv,"+lat_0=",(double)0.0)*DEG_TO_RAD,
                  getargdouble(argc,argv,"+lon_0=",(double)0.0)*DEG_TO_RAD,
                  getargdouble(argc,argv,"+x_0=",(double)0.0),
                  getargdouble(argc,argv,"+y_0=",(double)0.0));
               break;
      case 22: ierror = Set_Polyconic_Parameters(proja,projf,
                  getargdouble(argc,argv,"+lat_0=",(double)0.0)*DEG_TO_RAD,
                  getargdouble(argc,argv,"+lon_0=",(double)0.0)*DEG_TO_RAD,
                  getargdouble(argc,argv,"+x_0=",(double)0.0),
                  getargdouble(argc,argv,"+y_0=",(double)0.0));
               break;
      case 24: ierror = Set_Sinusoidal_Parameters(proja,projf,
                  getargdouble(argc,argv,"+lon_0=",(double)0.0)*DEG_TO_RAD,
                  getargdouble(argc,argv,"+x_0=",(double)0.0),
                  getargdouble(argc,argv,"+y_0=",(double)0.0));
               break;
      case 25: ierror = Set_Van_der_Grinten_Parameters(proja,projf,
                  getargdouble(argc,argv,"+lon_0=",(double)0.0)*DEG_TO_RAD,
                  getargdouble(argc,argv,"+x_0=",(double)0.0),
                  getargdouble(argc,argv,"+y_0=",(double)0.0));
               break;
      case 28: ierror = Set_UTM_Parameters(proja,projf,utmzoneoverride);
               break;
      }
    if (ierror!=0)
       {
       printf("error number %d\n",(int)ierror);
       zmabend("TEC projection initialization error");
       }
    
   /* apply the mapping to the data file */
    
   for (j=0;j<clen;j++)
      {
      u1 = xyzin[0][j];
      v1 = xyzin[1][j];
      if (!geotransinverse)
         {
         u1 *= DEG_TO_RAD;
         v1 *= DEG_TO_RAD;
         switch(nproj)
            {
            case  1:
            case 27: ierror = Convert_Geodetic_To_Transverse_Mercator(v1,u1,&u2,&v2);
                     break;
            case  7: ierror = Convert_Geodetic_To_Mercator(v1,u1,&u2,&v2);
                     break;
            case  8: ierror = Convert_Geodetic_To_Lambert(v1,u1,&u2,&v2);
                     break;
            case 11: ierror = Convert_Geodetic_To_Albers(v1,u1,&u2,&v2);
                     break;
            case 15: ierror = Convert_Geodetic_To_UPS(v1,u1,&hemisphere,&u2,&v2);
                     break;
            case 17: ierror = Convert_Geodetic_To_Equidistant_Cyl(v1,u1,&u2,&v2);
                     break;
            case 18: ierror = Convert_Geodetic_To_Cassini(v1,u1,&u2,&v2);
                     break;
            case 20: ierror = Convert_Geodetic_To_Miller(v1,u1,&u2,&v2);
                     break;
            case 21: ierror = Convert_Geodetic_To_Orthographic(v1,u1,&u2,&v2);
                     break;
            case 22: ierror = Convert_Geodetic_To_Polyconic(v1,u1,&u2,&v2);
                     break;
            case 24: ierror = Convert_Geodetic_To_Van_der_Grinten(v1,u1,&u2,&v2);
                     break;
            case 25: ierror = Convert_Geodetic_To_Sinusoidal(v1,u1,&u2,&v2);
                     break;
            case 28: ierror = Convert_Geodetic_To_UTM(v1,u1,&utmzone,
                        &hemisphere,&u2,&v2);
                     /* alz fix for N handling of S, astermos and lsat mos procs*/
                     if (v1<0.0&&hemisphereoverride=='N') v2 -= 10000000.0; 
                     break;
            }
         }
      else
         {
         switch(nproj)
            {
            case  1:
            case 27: ierror = Convert_Transverse_Mercator_To_Geodetic(u1,v1,&v2,&u2);
                     break;
            case  7: ierror = Convert_Mercator_To_Geodetic(u1,v1,&v2,&u2);
                     break;
            case  8: ierror = Convert_Lambert_To_Geodetic(u1,v1,&v2,&u2);
                     break;
            case 11: ierror = Convert_Albers_To_Geodetic(u1,v1,&v2,&u2);
                     break;
            case 15: ierror = Convert_UPS_To_Geodetic(hemisphereoverride,u1,v1,&v2,&u2);
                     break;
            case 17: ierror = Convert_Equidistant_Cyl_To_Geodetic(u1,v1,&v2,&u2);
                     break;
            case 18: ierror = Convert_Cassini_To_Geodetic(u1,v1,&v2,&u2);
                     break;
            case 20: ierror = Convert_Miller_To_Geodetic(u1,v1,&v2,&u2);
                     break;
            case 21: ierror = Convert_Orthographic_To_Geodetic(u1,v1,&v2,&u2);
                     break;
            case 22: ierror = Convert_Polyconic_To_Geodetic(u1,v1,&v2,&u2);
                     break;
            case 24: ierror = Convert_Sinusoidal_To_Geodetic(u1,v1,&v2,&u2);
                     break;
            case 25: ierror = Convert_Van_der_Grinten_To_Geodetic(u1,v1,&v2,&u2);
                     break;
            case 28: ierror = Convert_UTM_To_Geodetic(utmzoneoverride,
                        hemisphereoverride,u1,v1,&v2,&u2);
                     break;
            }
         u2 *= RAD_TO_DEG;
         v2 *= RAD_TO_DEG;
         }
      if (ierror!=0)
         {
         printf("error number %d\n",(int)ierror);
         zmabend("TEC coordinate projection error");
         }
      xyzout[0][j] = u2;
      xyzout[1][j] = v2;
      xyzout[2][j] = xyzin[2][j];
      }
}

void main44(void)
{
   int i,j,incol[3],outcol[3],unit,incount,outcount,coldef;
   int ibis,status,nl,ns,dummy1,dummy2,usegeotrans;
   long ellipseerror;
   char *labelstr1,*labelstr2;
   
   int pargc,domapp,indatum,outdatum;
   char *pargv[50];
   
   zifmessage("gtproj version Wed Oct 29 2008");
   
   /* get the basic parameters */
   
   zvparm("incol",incol,&incount,&coldef,20,0);
   zvparm("outcol",outcol,&outcount,&coldef,20,0);
   if (incount!=2&&incount!=3)
      zmabend("Parameter INCOL must be 2 columns, or 3 if (x,y,z)");
   if (outcount!=2&&outcount!=3)
      zmabend("Parameter OUTCOL must be 2 columns, or 3 if (x,y,z)");
   status = zvparm("ep_par",ep_par,&dummy1,&dummy2,1,0);
   if (zvptst("eposc")) usegeotrans = 0;
   if (zvptst("tec")) usegeotrans = 1;
         
   /* read in data from the ibis interface file */

   status = zvunit(&unit,"inp",1, NULL);
   status = IBISFileOpen(unit,&ibis,"update",0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nr",&clen,1,1,0);
   
   mz_alloc2((unsigned char ***)&xyzin,3,clen,8);
   mz_alloc2((unsigned char ***)&xyzout,3,clen,8);
   
   for (i=0;i<incount;i++)
      {
      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",incol[i]);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnRead(ibis,(char*)xyzin[i],incol[i],1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
      }
   if (incount==2) for (j=0;j<clen;j++) xyzin[2][j] = 0.0;
   
   /* initialize the NIMA datum table */
   
   NIMAerrcode = Initialize_Datums();
   NIMAerrcode = Datum_Count(&NIMAcount);
   /*printf("nima datum count %d\n",NIMAcount); was 230*/
   
   /* initialize ellipse */
   
   ellipseerror = Initialize_Ellipsoids();
   if (ellipseerror!=0)
   {
      printf("***ellipse error: %ld\n", ellipseerror);
      zmabend("Error in Ellipsoid table data");
   }
   
   /* get the GeoTIFF mappings */
   
   status = gtgetlab("inp",2,&labelstr1,&nl,&ns);
   if (status!=1) zmabend("Failed to read first GeoTIFF label");
   printf("\nInput GeoTIFF label:\n%s\n",labelstr1);
   
   status = gtgetlab("inp",3,&labelstr2,&nl,&ns);
   if (status!=1) zmabend("Failed to read second GeoTIFF label");
   printf("Output GeoTIFF label:\n%s\n",labelstr2);
   
   /* eposc translation labelstr1; do inverse if input is projected */
   
   for (i=0;i<clen;i++) /* for the no mapping case */
      {
      xyzout[0][i] = xyzin[0][i];
      xyzout[1][i] = xyzin[1][i];
      xyzout[2][i] = xyzin[2][i];
      }
   
   GeotiffToEposc(labelstr1,&pargc,pargv,&domapp,1,&indatum);
   if (domapp)
      {
      geotranssub(pargc,pargv);
      for (i=0;i<clen;i++)
         {
         xyzin[0][i] = xyzout[0][i];
         xyzin[1][i] = xyzout[1][i];
         xyzout[2][i] = xyzin[2][i];
         }
      }
   
   /* datum shift goes here; eposc translation labelstr2 needed for this */
   
   GeotiffToEposc(labelstr2,&pargc,pargv,&domapp,0,&outdatum);
   
   if (indatum==outdatum) printf("\ndatum shift not applied\n");
   else
      {
      NIMAdatumSub(indatum,outdatum);
      for (i=0;i<clen;i++)
         {
         xyzin[0][i] = xyzout[0][i];
         xyzin[1][i] = xyzout[1][i];
         xyzout[2][i] = xyzin[2][i];
         }
      printf("\ndatum shift applied, NIMA indices %d %d\n",indatum,outdatum);
      }
   /* do forward if output is projected */
   
   if (domapp) geotranssub(pargc,pargv);
   
   /* Output desired columns to the ibis interface file */
   
   for (i=0;i<outcount;i++)
      {
      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",outcol[i]);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnWrite(ibis,(char*)xyzout[i],outcol[i],1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
      }
       
   status = IBISFileClose(ibis,0);
   if (status!=1) IBISSignal(ibis,status,1);
  
   return;
}
