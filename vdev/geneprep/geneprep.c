#include "vicmain_c.h"
#include "applic.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "carto/ImageUtils.h"
#include "gsl/gsl_matrix.h"
#include "gsl/gsl_vector.h"
#include "carto/cartoLinkedList.h"
#include "carto/ibishelper.h"

typedef struct
{
   char name[20];
   int id;
   LINKEDLIST *dns;
   double **ratios;
   double *means;
   double *cov;
}CLASS;

/* prototypes */
void printClass(CLASS *c);
int printClass1(CLASS *c);
void printClass2(CLASS *c);
void printClasses(LINKEDLIST *classes, int printType);
CLASS* findClass(LINKEDLIST* classes, int id);
CLASS* createClass(int id, gsl_vector *dn);
void calcRatios(CLASS **c);
double getMean(double *samples, int n);
double getCov(double *s1, double *s2, double mean1, double mean2, int n);
void calcStats(CLASS **c);
LINKEDLIST* getClasses(void);
void write2IBIS(LINKEDLIST *classes);
void genTrainingDatFiles(LINKEDLIST *classes);

LINKEDLIST* LinkedList_getLinkedList(void);

/**********************************************/
void printClass(CLASS *c)
{
   int i, j;
   struct node *n;
    char message[100];

   zvmessage("**********\n"," ");
   sprintf(message,"id: %d\n", c->id);
    zvmessage(message," ");
   n = c->dns->head;
   for(i = 0; i < c->dns->size; i++)
   {
      gsl_vector *dn = (gsl_vector*)(n->data);

      zvmessage("dn: "," ");
      for(j = 0; (int)j < (int)dn->size; j++) {
        sprintf(message,"%lf ", gsl_vector_get(dn, (size_t)j));
        zvmessage(message," ");
      }
     zvmessage("\n"," ");
      zvmessage("ratios: "," ");
      for(j = 0; j < 28; j++) {
        sprintf(message,"%lf ", c->ratios[j][i]);
        zvmessage(message," ");
      }
      zvmessage("\n"," ");
      n = n->next;
   }

   zvmessage("means: "," ");
   
    for(i = 0; i < 28; i++) {
        sprintf(message,"%lf ", c->means[i]);
        zvmessage (message," ");
        zvmessage("\n"," ");
    }
}



/**********************************************/
int printClass1(CLASS *c)
{
    char message[100];

   sprintf(message,"id: %d\tcnt: %d\n", c->id, c->dns->size);
    zvmessage(message," ");
   return c->dns->size;
}

/**********************************************/
void printClass2(CLASS *c)
{
   int i, j, index;
    char message[100];

   zvmessage("**********\n"," ");
   sprintf(message,"id: %d\n", c->id);
    zvmessage(message," ");
   zvmessage("cov:\n"," ");
   index = 0;
   for(i = 0; i < 28; i++)
   {
      for(j = 0; j <= i; j++) {
         sprintf(message,"%lf ", c->cov[index++]);
         zvmessage(message," ");
         zvmessage("\n"," ");
        }
   }
   zvmessage("\n"," ");
}

/**********************************************/
void printClasses(LINKEDLIST *classes, int printType)
{
   int i, sum;
      struct node* n;
    char message[100];

   sum = 0;
   n = classes->head;
   if(printType == 1) zvmessage("**********\n"," ");
   for(i = 0; i < classes->size; i++)
   {
      if(printType == 0)
         printClass((CLASS*)(n->data));
      else if(printType == 1)
         sum += printClass1((CLASS*)(n->data));
      else if(printType == 2)
         printClass2((CLASS*)(n->data));
      n = n->next;
   }

   if(printType == 1) {
        sprintf(message,"SUM: %d\n", sum);
        zvmessage(message," ");
    }
}

/**********************************************/
CLASS* findClass(LINKEDLIST* classes, int id)
{
   int i;
   struct node *n;

   //   printf("\nclasses size: %d looking for %d\n", classes->size, id);
   n = classes->head;
   for(i = 0; i < classes->size; i++)
   {
      CLASS *c = (CLASS*)(n->data);

      //      printf("reading id: %d %d\n", c->id, id);

      if(c->id == id) return c;
      n = n->next;
   }

   //   printf("not found\n\n");
   return NULL;
}

/**********************************************/
CLASS* createClass(int id, gsl_vector *dn)
{
   CLASS *class;

   //   printf("creating %d\n", id);

   class = (CLASS*)malloc(sizeof(CLASS));
   class->id = id;
   class->dns = LinkedList_getLinkedList();
   LinkedList_add(class->dns, dn);

   return class;
}

/**********************************************/
void calcRatios(CLASS **c)
{
   int i, j, k;
   struct node* n;

   (*c)->ratios = (double**)malloc(28*sizeof(double*));
   for(i = 0; i < 28; i++) ((*c)->ratios)[i] = (double*)calloc((size_t)(*c)->dns->size, sizeof(double));

   n = (*c)->dns->head;
   for(i = 0; i < (*c)->dns->size; i++)
   {
      gsl_vector *v;
      int index = 0;

      v = (gsl_vector*)(n->data);
      for(j = 0; j < 8; j++)
      {
         for(k = j+1; k < 8; k++)
         {
            double dn1, dn2;

            dn1 = gsl_vector_get(v, (size_t)j);
            dn2 = gsl_vector_get(v, (size_t)k);
            (*c)->ratios[index++][i] = ((dn1 - dn2)/(double)(dn1 + dn2) + 1)*100;
         }
      }
      assert(index == 28);
      n = n->next;
   }
}

/**********************************************/
double getMean(double *samples, int n)
{
   int i;
   double sum;

   sum = 0.;
   for(i = 0; i < n; i++) sum += samples[i];

   return sum/(double)n;
}

/**********************************************/
double getCov(double *s1, double *s2, double mean1, double mean2, int n)
{
   int i;
   double sum;

   sum = 0.;
   for(i = 0; i < n; i++)
   {
      sum += (s1[i] - mean1)*(s2[i] - mean2);
      //      printf("s1: %lf mean1: %lf s2: %lf mean2: %lf sum: %lf\n", s1[i], mean1, s2[i], mean2, sum);
   }

   return sum/(double)(n);
}

/**********************************************/
void calcStats(CLASS **c)
{
   int i, j, index;

   // debug
   //   printf("\nclass: %d\n", (*c)->id);

   (*c)->means = (double*)calloc(28, sizeof(double));
   (*c)->cov = (double*)calloc(28*29/2, sizeof(double));

   for(i = 0; i < 28; i++)
   {
      (*c)->means[i] = getMean((*c)->ratios[i],(*c)->dns->size);
      //      printf("ratio: %lf gsl mean: %lf\n", (*c)->ratios[i][0], (*c)->means[i]);
   }

   index = 0;
   for(i = 0; i < 28; i++)
      for(j = 0; j <= i; j++)
         (*c)->cov[index++] = getCov((*c)->ratios[i], (*c)->ratios[j], (*c)->means[i], (*c)->means[j], (*c)->dns->size);
   assert(index == 28*29/2);
}

/**********************************************/
LINKEDLIST* getClasses()
{
   IBISStruct *inp;
   int i, j, minSizeThresh, dumdef, status, removed;
   LINKEDLIST *classes;
   char buf[20];
   struct node *n;

   inp = IBISHELPER_openIBIS("inp", 1, "read");
   classes = LinkedList_getLinkedList();
   //   buf = (char*)malloc(sizeof(char)*20);

   for(i = 0; i < inp->nr; i++)
   {
      int id;
      CLASS *c;
      gsl_vector *dn;

      dn = gsl_vector_alloc(8);
      for(j = 0; j < 8; j++) gsl_vector_set(dn, (size_t)j, IBISHELPER_getDouble(inp, j+3, i));
      IBISHELPER_getString(inp, buf, 0, i);
      sscanf(buf, "CLASS %d", &id);
      c = findClass(classes, id);
      if(c == NULL)
      {
         c = createClass(id, dn);
         strcpy(c->name, buf);

         //printf("outside %d\n", c->id);
         LinkedList_add(classes, c);
      }
      else LinkedList_add(c->dns, dn);
   }

   //   printf("-----> BEFORE MIN THRESH APPLIED <-----\n");
   //   printClasses(classes, 1);

   status = zvp("minsize", &minSizeThresh, &dumdef);
   if(status != 1) zmabend("Error while acquiring minsize.\n");

   removed = 1;
   while(removed)
   {
      removed = 0;
      n = classes->head;
      for(i = 0; i < classes->size; i++)
      {
         CLASS *c = (CLASS*)(n->data);

         if(c->dns->size < minSizeThresh)
         {
           //            printf("Class %d thrown out\n", c->id);
            LinkedList_removeNode(classes, n);
            removed = 1;
            break;
         }
         n = n->next;
      }
   }

   n = classes->head;
   for(i = 0; i < classes->size; i++)
   {
      CLASS *c;

      c = (CLASS*)(n->data);

      // calculate ratios
      calcRatios(&c);
      // calculate statistics
      calcStats(&c);
      n = n->next;
   }

   //   printf("-----> AFTER MIN THRESH APPLIED <-----\n");
   //   printClasses(classes, 1);
   //   printClasses(classes, 2);

   IBISHELPER_closeIBIS(&inp);
   return classes;
}

/**********************************************/
void write2IBIS(LINKEDLIST *classes)
{
   int nc, i, j;
   char **formats;
//   double **samples;
//   int nSamples;
   IBISStruct *out;
   struct node *n;

   nc = 1 + 2 + 28 + (28*29)/2;
   formats = (char**)malloc((long unsigned int)nc*sizeof(char*));
   for(i = 0; i < nc; i++)
   {
      formats[i] = (char*)malloc(6*sizeof(char));
      if(i == 0) strcpy(formats[i], "A12");
      else strcpy(formats[i], "real");
   }

   out = IBISHELPER_openIBIS_out(formats, 1, classes->size, nc);

   n = classes->head;
   for(i = 0; i < classes->size; i++)
   {
      int index;
      CLASS *c;

      c = (CLASS*)(n->data);

      index = 0;
      IBISHELPER_setString(out, index++, i, c->name);
      //      printf("num of dns: %d\n", c->dns->size);
      IBISHELPER_setDouble(out, index++, i, (double)(c->dns->size));
      IBISHELPER_setDouble(out, index++, i, 28.);
      for(j = 0; j < 28; j++) IBISHELPER_setDouble(out, index++, i, c->means[j]);
      for(j = 0; j < 28*29/2; j++) IBISHELPER_setDouble(out, index++, i, c->cov[j]);

      assert(index == nc);
      n = n->next;
   }

   IBISHELPER_closeIBIS(&out);

   for(i = 0; i < nc; i++) free(formats[i]);
   free(formats);
}

/**********************************************/
void genTrainingDatFiles(LINKEDLIST *classes)
{
   int i;
   struct node *n;

   n = classes->head;
   for(i = 0; i < classes->size; i++)
   {
      IBISPrep *prep;
      IBISStruct *ibis;
      CLASS *c;
      char fname[300];
      int j, k;

      c = (CLASS*)(n->data);
      //      printClass1(c);
      sprintf(fname, "%d.int", c->id);
      prep = IBISHELPER_openIBIS_out2(fname, i+1, c->dns->size);
      for(j = 0; j < 29; j++) IBISHELPER_addColumn(prep, "REAL");
      ibis = IBISHELPER_getIBISStruct(&prep);

      for(j = 0; j < c->dns->size; j++)
      {
         IBISHELPER_setDouble(ibis, 0, j, c->id);
         //         printf("%d\n", j);
         for(k = 0; k < 28; k++) IBISHELPER_setDouble(ibis, k+1, j, (c->ratios)[k][j]);
      }

      IBISHELPER_closeIBIS(&ibis);
      n = n->next;
   }
}

/**********************************************/
void main44(void)
{
//   int genTrainingDataFiles;

    zvmessage("geneprep - 10-09-2019 - rjb - 64-bit"," ");
   LINKEDLIST *classes = getClasses();
   //   printClasses(classes, 0);
   write2IBIS(classes);
   if(zvptst("TrainFiles")) genTrainingDatFiles(classes);
}
