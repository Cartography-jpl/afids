#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "carto/shapefil.h"

#include "carto/cartoVicarProtos.h"

void loadFeatureNames (char * path);
char * featureName (int code);

void main44(void)
{
   int uVertices, uFeatureCodes, iVertices, iFeatureCodes, status;
   int nc;
   double * col1, * col2, * codes=NULL;
   int nVertices, nFeatures, nFeatureCodes;
   char inpfilenames [3] [99];
   char outPrefix [99];
   char shapeType [99];
   int parmct, parmdf, swap;
   int * featureVertexCounts;
   int inpCount;
   int allowNullVectors;
           
   zifmessage("vic2shp version Thu Jan  3 2008");

   zvparm ("inp", inpfilenames, &inpCount, &parmdf, 3, 99);
   zvparm ("out", outPrefix, &parmct, &parmdf, 1, 99);
   zvparm ("shape", shapeType, &parmct, &parmdf, 1, 99);
   swap = zvptst("swap");
   zvp ("noNullVec", & allowNullVectors, & parmct);
   allowNullVectors = ! allowNullVectors;

   printf ("out=%s\n", outPrefix);

   if (strcmp (shapeType, "arc") && strcmp (shapeType, "polygon"))
     zmabend ("shape parm must be either arc or polygon");

   if (inpCount == 3)
     loadFeatureNames (inpfilenames [2]);

   if (zvunit (&uVertices, "inp", 1, NULL) != 1)
     zmabend ("zvunit failed on vertex file");
   if ((status = IBISFileOpen (uVertices, &iVertices, "read", 0, 0, 0, 0)) != 1)
     IBISSignalU (uVertices, status, 1);

   if (inpCount > 1) {
     if (zvunit (&uFeatureCodes, "inp", 2, NULL) != 1)
       zmabend ("zvunit failed on feature code file");
     if ((status = IBISFileOpen (uFeatureCodes, &iFeatureCodes, "read", 0, 0, 0, 0)) != 1)
       IBISSignalU (uFeatureCodes, status, 1);
   }

   IBISFileGet (iVertices, "nr", &nVertices, 1, 1, 0);
   if (inpCount > 1)
     IBISFileGet (iFeatureCodes, "nr", &nFeatureCodes, 1, 1, 0);

   IBISFileGet (iVertices, "nc", &nc, 1, 1, 0);
   if (nc != 2)
     zmabend ("expected two column IBIS vertex file");
   if (inpCount > 1) {
     IBISFileGet (iFeatureCodes, "nc", &nc, 1, 1, 0);
     if (nc != 2)
       zmabend ("expected two column IBIS feature code file");
   }

   col1 = (double *) malloc (sizeof (double) * nVertices);
   col2 = (double *) malloc (sizeof (double) * nVertices);
   if (inpCount > 1)
     codes = (double *) malloc (sizeof (double) * nFeatureCodes);

   if ((status = IBISColumnSet (iVertices, "U_FORMAT", "DOUB", 1)) != 1)
     IBISSignal (iVertices, status, 1);
   if ((status = IBISColumnRead (iVertices, (char *) col1, 1, 1, nVertices)) != 1)
     IBISSignal (iVertices, status, 1);

   if ((status = IBISColumnSet (iVertices, "U_FORMAT", "DOUB", 2)) != 1)
     IBISSignal (iVertices, status, 1);
   if ((status = IBISColumnRead (iVertices, (char *) col2, 2, 1, nVertices)) != 1)
     IBISSignal (iVertices, status, 1);

   if (inpCount > 1) {
     if ((status = IBISColumnSet (iFeatureCodes, "U_FORMAT", "DOUB", 1)) != 1)
       IBISSignal (iFeatureCodes, status, 1);
     if ((status = IBISColumnRead (iFeatureCodes, (char *) codes, 1, 1, nFeatureCodes)) != 1)
       IBISSignal (iFeatureCodes, status, 1);
   }

   if ((status = IBISFileClose (iVertices, 0)) != 1)
     IBISSignal (iVertices, status, 1);
   if (inpCount > 1)
     if ((status = IBISFileClose (iFeatureCodes, 0)) != 1)
       IBISSignal (iFeatureCodes, status, 1);
  
   /* Count the number of features in the table, by counting nulls */
   {
     int i;
     int featCount = 0;
     int lastNull = -2;
     for (i = 0; i < nVertices; i ++) {
       while (i < nVertices && (col1 [i] != 0.0 || col2 [i] != 0.0)) {
	 i ++;			/* skip non nulls */
       }

       /* either we're out of vertices, or we hit a null */
       if (i >= nVertices) {	/* we ran out of vertices, so didn't see the last null */
	 break;
       }

       if (allowNullVectors ||	/* just a null can be a feature */
	   i > lastNull + 1)	/* we saw a vertex before the null */
	 featCount ++;		/* count feature */
       else {
	 /* we don't allow null vectors and we saw a double null, so
            this is the end of the table, regardless of nFeatures'
            value, if available */
	 break;
       }

       lastNull = i;
     }

     nFeatures = featCount;
   }   

   if (inpCount > 1)
     nFeatures = MIN (nFeatures, nFeatureCodes);

   featureVertexCounts = (int *) malloc (sizeof (int) * nFeatures);

   {
     int i;
     int featureCount = 0;
     int firstVertex;
     for (i = 0; i < nVertices; i ++) {
       firstVertex = i;
       while (i < nVertices && (col1 [i] != 0.0 || col2 [i] != 0.0)) {
	 i ++;
       }
       featureVertexCounts [featureCount] = i - firstVertex;

       featureCount ++;
       if (featureCount >= nFeatures)
	 break;
     }
   }   

   {
     SHPHandle shapeHandle;
     SHPObject * shapeObject;
     DBFHandle dbfHandle;
     int nParts = 1;
     int panParts = 0;
     int i;
     int featureCount = 0;
     int firstVertex;
     char dbfName [110];

     /* create the shape files */
     shapeHandle = SHPCreate (outPrefix, strcmp (shapeType, "arc") ? SHPT_POLYGON : SHPT_ARC);
  
     for (i = 0; i < nVertices; i ++) {
       firstVertex = i;
       while (i < nVertices && (col1 [i] != 0.0 || col2 [i] != 0.0)) {
	 i ++;
       }

       if (swap)		/* swap column order */
	 shapeObject = SHPCreateObject (strcmp (shapeType, "arc") ? SHPT_POLYGON : SHPT_ARC,
					-1, nParts, & panParts, NULL, featureVertexCounts [featureCount], col2 + firstVertex, col1 + firstVertex, NULL, NULL);
       else
	 shapeObject = SHPCreateObject (strcmp (shapeType, "arc") ? SHPT_POLYGON : SHPT_ARC,
					-1, nParts, & panParts, NULL, featureVertexCounts [featureCount], col1 + firstVertex, col2 + firstVertex, NULL, NULL);
       SHPWriteObject (shapeHandle, -1, shapeObject);
       SHPDestroyObject (shapeObject);

       featureCount ++;
       if (featureCount >= nFeatures)
	 break;
     }
    
     SHPClose (shapeHandle);

     if (inpCount > 1) {
       /* create the feature code dBase file */
       sprintf (dbfName, "%s.dbf", outPrefix);
       dbfHandle = DBFCreate (dbfName);

       DBFAddField (dbfHandle, "FCODE", FTString, 15, 0);

       if (inpCount == 2)	/* don't have the featureName */
	 for (i = 0; i < featureCount; i ++) {
	   char buf [10];
	   sprintf (buf, "%f", codes [i]);
	   DBFWriteStringAttribute (dbfHandle, i, 0, buf);
	 } /* inpCount must be == 3 */
       else			/* do have the featureName */
	 for (i = 0; i < featureCount; i ++)
	   DBFWriteStringAttribute (dbfHandle, i, 0, featureName (codes [i]));

       DBFClose (dbfHandle);
     }
   }
}

static int * featureCodes;
static char ** featureNames;
static int nameCount;

void loadFeatureNames (char * path) {
  FILE * f;
  double code;
  char name [20];

  nameCount = 0;
  
  if ((f = fopen (path, "r")) == 0)
    zmabend ("error opening feature name file");

  while (! feof (f)) {
    if (fscanf (f, "%lf %s", &code, name) == 2)
      nameCount ++;
  }
  fclose (f);

  featureCodes = (int *) malloc (sizeof (int) * nameCount);
  featureNames = (char **) malloc (sizeof (char *) * nameCount);
  
  nameCount = 0;
  f = fopen (path, "r");
  while (! feof (f)) {
    if (fscanf (f, "%lf %s", &code, name) == 2) {
      featureCodes [nameCount] = code;
      featureNames [nameCount] = strdup (name);
      nameCount ++;
    }
  }
}

char * featureName (int code) {
  int i;
  for (i = 0; i < nameCount; i ++)
    if (featureCodes [i] == code)
      return featureNames [i];

  return "";
}
