#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <assert.h>

#include "carto/ibishelper.h"
#include "carto/cartoClassUtils.h"
#include "carto/cartoSortUtils.h"

/* prototypes */
void getHighestID(IBISStruct *inp, char* highestID);
void generateOutFile(CLASS *class, int num);


/****************************************************************/
/*
void getHighestID(IBISStruct *inp, char* highestID)
{
   int i;
   int highestFrequency, frequency;

   highestFrequency = IBISHELPER_getInt(inp, 1, 0);
   IBISHELPER_getString(inp, highestID, 0, 0);
   for(i = 1; i < inp->nr; i++)
   {
      frequency = IBISHELPER_getInt(inp, 1, i);
      if(frequency > highestFrequency)
      {
         highestFrequency = frequency;
         IBISHELPER_getString(inp, highestID, 0, i);
      }
   }
}
*/

/****************************************************************/
void generateOutFile(CLASS *class, int num)
{
   int j, k, rowIndex, id;
   IBISStruct *out;
   IBISPrep *prep;
   char name[200];

   sscanf(class->name, "CLASS %d", &id);
   //   printf("name: %s id: %d\n", class->name, id);
   sprintf(name, "bkrdplot_%d_%d_%d.ibis", num, id, class->nSamples);
   prep = IBISHELPER_openIBIS_out2(name, num, 28);
   //   printf("here 1\n");
   IBISHELPER_addColumn(prep, "REAL");
   IBISHELPER_addColumn(prep, "REAL");
   IBISHELPER_addColumn(prep, "A12");
   IBISHELPER_addColumn(prep, "REAL");
   IBISHELPER_addColumn(prep, "REAL");

   //   printf("here 2\n");
   out = IBISHELPER_getIBISStruct(&prep);
   //   printf("here 3\n");
   rowIndex = 0;
   for(j = 0; j < 8; j++)
   {
      for(k = j+1; k < 8; k++)
      {
         double mean, sig;

         mean = gsl_vector_get(class->means, (size_t)rowIndex);
         sig = gsl_matrix_get(class->cov_matrix, (size_t)rowIndex, (size_t)rowIndex);

         IBISHELPER_setDouble(out, 0, rowIndex, (double)(j+1));
         IBISHELPER_setDouble(out, 1, rowIndex, (double)(k+1));
         IBISHELPER_setString(out, 2, rowIndex, class->name);
         IBISHELPER_setDouble(out, 3, rowIndex, mean);
         IBISHELPER_setDouble(out, 4, rowIndex, sig);
         ++rowIndex;
      }
   }

   IBISHELPER_closeIBIS(&out);
}

/****************************************************************/
void main44(void)
{
   int i, nClasses, *sortedIndices, *numOfSamples;

    
   IBISStruct *inp;
   CLASS **classes;

    zvmessage ("plotbkrd - Oct 9, 2019 - rjb (64-bit)"," ");
   inp = IBISHELPER_openIBIS("inp", 1, "read");
   classes = CARTOCLASS_loadClassesFromIBIS(inp);
   nClasses = inp->nr;
   IBISHELPER_closeIBIS(&inp);


   numOfSamples = (int*)malloc(sizeof(int)*(long unsigned int)nClasses);
   for(i = 0; i < nClasses; i++)
      numOfSamples[i] = classes[i]->nSamples;

   sortedIndices = (int*)malloc(sizeof(int)*(long unsigned int)nClasses);
   for(i = 0; i < nClasses; i++) sortedIndices[i] = i;

   getSelectionSortIndices(numOfSamples, sortedIndices, nClasses, CART_INT);

   for(i = 0; i < nClasses; i++) generateOutFile(classes[sortedIndices[nClasses - 1 - i]], i+1);
}
