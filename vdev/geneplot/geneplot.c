#include "vicmain_c.h"
#include "applic.h"
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "carto/ibishelper.h"
/* prototypes */
int getIndex(int pre, int post);
void outputRunScript(char **datFnames, int inpCnt);
char** outputDat(char datFnames[100][300], IBISStruct **inps, int inpCnt);

/* prototypes */
int getIndex(int pre, int post);
void outputRunScript(char **datFnames, int inpCnt);
char** outputDat(char datFnames[100][300], IBISStruct **inps, int inpCnt);



/**********************************************/
int getIndex(int pre, int post)
{
   int i, j, index;

   index = 0;
   for(i = 0; i < 8; i++)
   {
      for(j = i+1; j < 8; j++)
      {
         index++;
         if(i+1 == pre && j+1 == post)
            return index;
      }
   }

   return 0;
}

/**********************************************/
void outputRunScript(char **datFnames, int inpCnt)
{
   int status, dumcnt, dumdef, i, j;
   FILE *f;
   char outputString[1000];
   char outputFname[300];


   status = zvparm("out", outputFname, &dumcnt, &dumdef, 1, 300);
   if(status != 1) zmabend("Error while acquiring output gif file name");
   if(strcmp(outputFname+(strlen(outputFname)-4), ".gif") != 0)
      zmabend("output file name must end with .gif");

    // output gnuplot script commands into file called tmp_geneplot.pg
   //   printf("inside here1\n");
   f = fopen("tmp_geneplot.pg", "w");
   if(f == NULL) zmabend("Error while opening tmp_geneplot.pg.");
   strcpy(outputString, "set term gif\n");
   fwrite(outputString, sizeof(char), strlen(outputString), f);
   sprintf(outputString, "set output '%s'\n", outputFname);
   fwrite(outputString, sizeof(char), strlen(outputString), f);
   strcpy(outputString, "set xrange [0:29]\n");
   fwrite(outputString, sizeof(char), strlen(outputString), f);
   strcpy(outputString, "set key outside\n");
   fwrite(outputString, sizeof(char), strlen(outputString), f);
   //   strcpy(outputString, "set yrange [60:130]\n");
   //   fwrite(outputString, sizeof(char), strlen(outputString), f);

   //   printf("inside here2\n");
   // write xtics
   sprintf(outputString, "set xtics rotate\n");
   fwrite(outputString, sizeof(char), strlen(outputString), f);
   sprintf(outputString, "set xtics (\n");
   for(i = 0; i < 8; i++)
   {
      for(j = i+1; j < 8; j++)
      {
        //         printf("%s\n", outputString);
         sprintf(outputString+(strlen(outputString)), "\"(%d-%d)\" %d", i+1, j+1, getIndex(i+1, j+1));
         if(i < 6 || j < 7) sprintf(outputString+(strlen(outputString)), ", ");
      }
   }
   sprintf(outputString+(strlen(outputString)), ")\n");
   fwrite(outputString, sizeof(char), strlen(outputString), f);

   //   printf("inside here3\n");
   strcpy(outputString, "set size 1,1\n");
   fwrite(outputString, sizeof(char), strlen(outputString), f);
   strcpy(outputString, "plot ");
   sprintf(outputString, "%s\"%s\" with errorbars", outputString, datFnames[0]);
   //   printf("inside here4\n");
   for(i = 1; i < inpCnt; i++)
   {
     //      printf("inside here5 %d\n", i);
      sprintf(outputString, "%s ,\"%s\" with errorbars", outputString, datFnames[i]);
   }
   strcat(outputString, "\n");
   fwrite(outputString, sizeof(char), strlen(outputString), f);

   //   printf("inside here4\n");
   fclose(f);
}

/**********************************************/
char** outputDat(char datFnames[100][300], IBISStruct **inps, int inpCnt)
{
   int i, j, k;
   int pre, post;
   double mean, sigma;
   char outputString[500];
   char **outputFname;
    char message[100];
   FILE *f;

    pre = 0;
    post = 0;
    mean = 0.0;
    sigma = 0.0;
   outputFname = (char**)malloc(sizeof(char*)*100);
   for(i = 0; i < 100; i++) outputFname[i] = (char*)calloc(300, sizeof(char));

   for(i = 0; i < inpCnt; i++)
   {
      char *tmp;

      //      printf("here\n");
      // string manipulation in C is such a pain
      if((tmp=strrchr(datFnames[i], '/')) == NULL)
         tmp = datFnames[i];
      else
         ++tmp;
      //      printf("hereb %s\n", tmp);
      if(strstr(tmp, "bkrdplot_") != NULL) tmp += strlen("bkrdplot_");
      if(strchr(tmp, '.') != NULL)
            strncpy(outputFname[i], tmp, strchr(tmp, '.')-tmp);
      else
 	 strcpy(outputFname[i], tmp);
      //      printf("herec\n");
      strcat(outputFname[i], ".dat");
      // printf("%s\n", outputFname[i]);
      f = fopen(outputFname[i], "w");
      if(f == NULL) {
            sprintf (message,"Error while opening %s\n",outputFname[i]);
            zmabend(message);
        }
        pre=0;
        post=0;
        mean=0.0;
        sigma=0.0;
      for(j = 0; j < inps[i]->nr; j++)
      {
         for(k = 0; k < 5; k++)
         {
            switch(k)
            {
               case 0:
                  pre = IBISHELPER_getInt(inps[i], k, j);
                  break;
               case 1:
                  post = IBISHELPER_getInt(inps[i], k, j);
                  break;
               case 2:
                  continue;
                    //                  id = IBISHELPER_getInt(inps[i], k, j);
               case 3:
                  mean = IBISHELPER_getDouble(inps[i], k, j);
                    break;
               case 4:
                  sigma = IBISHELPER_getDouble(inps[i], k, j);
                  break;
               default:
                zvmessage("switch statement k> 4 is invalid"," ");
                zmabend ("ABEND");
    
            }
         }
         sprintf(outputString, "%d %lf %lf\n", getIndex(pre, post), mean, sigma);
         fwrite(outputString, sizeof(char), strlen(outputString), f);
      }

      fclose(f);
   }

   return (char**)outputFname;
}

/**********************************************/
void main44(void)
{
   char datFnames[100][300];
   char **gnuDatFnames;
   int i, status, inpCnt, dumdef;
   IBISStruct **inps;

    zvmessage ("geneplot - 10-2-2019 - rjb  (64-bit)"," "); 
   //   printf("here1\n");
   status = zvparm("inp", datFnames, &inpCnt, &dumdef, 100, 300);
   //   printf("%d %s %s %s\n", inpCnt, datFnames[0], datFnames[1], datFnames[12]);
   if(status != 1) zmabend("Error while acquiring input ibis file names.");
   inps = (IBISStruct **)malloc(sizeof(IBISStruct*)*(long unsigned int)inpCnt);
   //   printf("here2\n");
   for(i = 0; i < inpCnt; i++)
      inps[i] = IBISHELPER_openIBIS("inp", i+1, "read");

   //   printf("here2\n");
   gnuDatFnames = outputDat(datFnames, inps, inpCnt);
   //   printf("here3\n");
   outputRunScript(gnuDatFnames, inpCnt);
   //   printf("here4\n");
}
