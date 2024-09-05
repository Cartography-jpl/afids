#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdio.h>
#include <string.h>

#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "carto/cartoMemUtils.h"
#include "carto/cartoVicarProtos.h"
#include "carto/cartoLsqUtils.h"

/*=========================================================*/

void doLstSq(concolv, xpar, ypar, clen, indcount, indcol, coeffcol, sout, noprint, rescol, rout)
   double **xpar, *ypar, **sout, *rout;
   float *concolv;
   int indcount, indcol[20], coeffcol[20], noprint, rescol;
{
   /* do the least squares */
   /* could reduce storage need by saving solutions more compactly, and
      then free the input data, and unpack the solutions */
   int uptr, i, j, lptr, igroup, ier, numlsq;
   float groupnbr;
   double sum, eps, csol[20];
   double *clsq, *clsqdep;

   mz_alloc1((unsigned char **)&clsq,indcount*clen,8);
   mz_alloc1((unsigned char **)&clsqdep,clen,8);

   uptr = 0;
   for (igroup=0;;igroup++)
   {
      lptr = uptr;
      if (lptr>=clen) break;
      groupnbr = concolv[lptr];
      for (uptr=lptr;uptr<=clen;uptr++)
         if (uptr==clen||concolv[uptr]!=groupnbr) break;
      numlsq = uptr-lptr;
      for (i=0;i<indcount;i++)
         for (j=lptr;j<uptr;j++)
            clsq[i*numlsq+j-lptr] = xpar[i][j];
      for (j=lptr;j<uptr;j++)
         clsqdep[j-lptr] = ypar[j];
      eps = 1.e-7;
      lsqfit(clsq,clsqdep,numlsq,indcount,csol,eps,&ier);
      
      /* printing if requested */
      
      if (!noprint)
      {
         printf("seq  concol  datcol    solution coefficient\n\n");
         for (i=0;i<indcount;i++)
            if (ier==0) printf("%3d %7.2f %7d %24.10f\n",
                i+1,groupnbr,indcol[i],csol[i]);
            else printf("%3d %7.2f %7d %24.10f\n",
                i+1,groupnbr,indcol[i],-999.0);
         printf("\n");
      }

      /* calculate the output data */
      
      if (coeffcol[0]!=0)
      {
         for (i=0;i<indcount;i++)
            for (j=lptr;j<uptr;j++)
               if (ier==0) sout[i][j] = csol[i];
               else sout[i][j] = -999.0;
      }
      if (rescol!=0)
      {
         for (j=lptr;j<uptr;j++)
            if (ier==0)
            {
               sum = 0.0;
               for (i=0;i<indcount;i++)
                  sum += csol[i]*xpar[i][j];
               rout[j] = ypar[j]-sum;
            }
            else rout[j] = 0.0;
      }
   }

   free(clsq);
   free(clsqdep);
}

/*=========================================================*/

int findOutlier1(rout, clen, thresh)
   double thresh, *rout;
   int clen;
{
   int i, max;

   max = 0;
   for(i = 1; i < clen; i++)
   {
      if(fabs(rout[i]) > fabs(rout[max]))
         max = i;
   }

   printf("**Max error: %f Threshold: %f", fabs(rout[max]), thresh);

   if(thresh < fabs(rout[max])) return max;
   return -1;
}

/*=========================================================*/
int findOutlier2(rout1, rout2, clen, thresh)
   double thresh, *rout1, *rout2;
   int clen;
{
   int i, max;
   double error, maxError;
   
   max = 0;
   maxError = pow(rout1[0]*rout1[0] + rout2[0]*rout2[0], 0.5);
   for(i = 1; i < clen; i++)
   {
      error = pow(rout1[i]*rout1[i] + rout2[i]*rout2[i], 0.5);
      if(error > maxError)
      {
         max = i;
	 maxError = error;
      }
   }

   printf("**Max error: %f Threshold: %f", maxError, thresh);

   if(maxError > thresh) return max;
   return -1;
}

/*=========================================================*/
void main44(void)
{
   int i,j,indcol[20],coeffcol[20],unit,unit2,indcount,coeffcount,coldef;
   int coeffcol2[20],coeffcount2, coldef2;
   int ibis1,ibis2,status,clen,nc,nr,depcol,rescol,concol;
   int depcol2,rescol2, *outliers;
   int dummy,noprint, outlierCnt, attempt, cnt, def;
   double thresh;
   float *concolv;
   double **xpar,**sout,*ypar,*rout;
   double **sout2,*ypar2,*rout2;
   double *inFile, *outFile;

   zifmessage("ibislsq3 version 24-oct-02");
   
   /* get the basic parameters */
   
   status = zvp("depcol",&depcol,&dummy);
   status = zvp("depcol2", &depcol2, &dummy);
   status = zvp("rescol",&rescol,&dummy);
   status = zvp("rescol2", &rescol2, &dummy);
   status = zvp("concol",&concol,&dummy);
   status = zvparmd("thresh", &thresh, &cnt, &def, 0, 0);
   noprint = zvptst("noprint");

   zvparm("indcol",indcol,&indcount,&coldef,20,0);
   zvparm("coeffcol",coeffcol,&coeffcount,&coldef,20,0);
   if(depcol2 != 0)
      zvparm("coeffcol2",coeffcol2,&coeffcount2,&coldef2,20,0);
   if(coeffcount!=indcount && coldef==0)
      zmabend("Count for parameter COEFFCOL wrong");
   if(depcol2 != 0 && coeffcount2!=indcount && coldef2==0)
      zmabend("Count for parameter COEFFCOL2 wrong");
   /* read in data from the ibis interface file */

   status = zvunit(&unit,"inp",1, NULL);
   status = IBISFileOpen(unit,&ibis1,"read",0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis1,"nr",&nr,1,1,0);
   IBISFileGet(ibis1,"nc",&nc,1,1,0);
   clen=nr;
   mz_alloc1((unsigned char **)&outliers, clen, sizeof(int));
   outlierCnt = 0;
   
   mz_alloc2((unsigned char ***)&xpar,indcount,clen,8);
   mz_alloc1((unsigned char **)&ypar,clen,8);
   if(depcol2 != 0)   mz_alloc1((unsigned char **)&ypar2,clen,8);
   mz_alloc1((unsigned char **)&concolv,clen,8);
   
   if (coeffcol[0]!=0) mz_alloc2((unsigned char ***)&sout,indcount,clen,8);
   if (depcol2!=0 && coeffcol2[0]!=0)
      mz_alloc2((unsigned char ***)&sout2,indcount,clen,8);
   if (rescol!=0) mz_alloc1((unsigned char **)&rout,clen,8);
   if(rescol2 != 0) mz_alloc1((unsigned char**)&rout2, clen, 8);
   
   for (i=0;i<indcount;i++)
   {
      status = IBISColumnSet(ibis1,"U_FORMAT","DOUB",indcol[i]);
      if (status!=1) IBISSignal(ibis1,status,1);
      status = IBISColumnRead(ibis1,(char *)(xpar[i]),indcol[i],1,clen);
      if (status!=1) IBISSignal(ibis1,status,1);
   }

   status = IBISColumnSet(ibis1,"U_FORMAT","DOUB",depcol);
   if (status!=1) IBISSignal(ibis1,status,1);
   status = IBISColumnRead(ibis1,(char *)ypar,depcol,1,clen);
   if (status!=1) IBISSignal(ibis1,status,1);

   if(depcol2 != 0)
   {
      status = IBISColumnSet(ibis1,"U_FORMAT","DOUB",depcol2);
      if (status!=1) IBISSignal(ibis1,status,1);
      status = IBISColumnRead(ibis1,(char *)ypar2,depcol2,1,clen);
      if (status!=1) IBISSignal(ibis1,status,1);
   }
   
   if (concol>0)
   {
      status = IBISColumnSet(ibis1,"U_FORMAT","REAL",concol);
      if (status!=1) IBISSignal(ibis1,status,1);
      status = IBISColumnRead(ibis1,(char *)concolv,concol,1,clen);
      if (status!=1) IBISSignal(ibis1,status,1);
   }
   else for (i=0;i<clen;i++) concolv[i] = 1.0;

   attempt = 0;
   doLstSq(concolv, xpar, ypar, clen, indcount, indcol, coeffcol, sout, noprint, rescol, rout);
   if(depcol2 != 0)
      doLstSq(concolv, xpar, ypar2, clen, indcount, indcol, coeffcol2, sout2, noprint, rescol2, rout2);

   if(depcol2 == 0 && thresh > .0)
      outliers[outlierCnt++] = findOutlier1(rout, clen, thresh);
   else if(thresh > .0)
      outliers[outlierCnt++] = findOutlier2(rout, rout2, clen, thresh);

   if(outliers[outlierCnt-1] >= 0 && thresh > 0.0)
      printf("  Point %d thrown out.  Attempt %d**\n", outliers[outlierCnt-1], outlierCnt);
   else if(thresh > 0.0)
      printf("  All points are under error threshold.**\n");

   while(thresh > .0 && outliers[outlierCnt-1] != -1)
   {
      --clen;
      for(i = outliers[outlierCnt-1]; i < clen; i++)
      {
	 concolv[i] = concolv[i+1];
	 ypar[i] = ypar[i+1];
	 for(j = 0; j < indcount; j++)
	    xpar[j][i] = xpar[j][i+1];
	 if(depcol2 > 0)
            ypar2[i] = ypar2[i+1];
      }

      doLstSq(concolv, xpar, ypar, clen, indcount, indcol, coeffcol, sout, noprint, rescol, rout);
      if(depcol2 != 0)
         doLstSq(concolv, xpar, ypar2, clen, indcount, indcol, coeffcol2, sout2, noprint, rescol2, rout2);

      if(depcol2==0)
         outliers[outlierCnt++] = findOutlier1(rout, clen, thresh);
      else
         outliers[outlierCnt++] = findOutlier2(rout, rout2, clen, thresh);

      if(outliers[outlierCnt-1] >= 0)
	 printf("  Point %d thrown out.  Attempt %d**\n", outliers[outlierCnt-1], outlierCnt);
      else
 	 printf("  All points are under error threshold.**\n");
   }

   /* Outlier indices are not what they were because of array shifting after
      throwing out each outlier.  We need to calculate the original index */
   {
      int oi, oj;

      for(oi = outlierCnt-2; oi > 0; oi--)
      {
	 int actualIndex = outliers[oi];
	 /*printf("index: %d outlier: %d\n", oi, outliers[oi]);*/
	 for(oj = oi-1; oj >= 0; oj--)
	 {

	    if(actualIndex >= outliers[oj]) 
            {
	       /*printf("(%d - %d) ", oj, outliers[oj]);*/
               actualIndex++;
	    }
	 }
	 outliers[oi] = actualIndex;
	 /*printf("final outlier: %d\n", outliers[oi]);*/
      }
   }

   /* quick check */
   /*printf("outlierCnt: %d clen: %d nr: %d\n", outlierCnt, clen, nr);*/
   if(outlierCnt > 0 && (outlierCnt-1)+clen != nr)
      zmabend("Some entities were lost.\n");

   /* Output desired columns to the ibis interface file */
   zvselpi(0);

   status = zvunit(&unit2,"out",1, NULL);
   if (status!=1) IBISSignalU(unit2,status,1);
   status = IBISFileUnit(unit2, &ibis2, "write", nc, clen, 0, 0);
   if (status!=1) IBISSignalU(unit2,status,1);
   status = IBISFileSet(ibis2, "fmt_default", "doub", 1);
   if (status!=1) IBISSignalU(unit2,status,1);
   status = IBISFileUnitOpen(ibis2);
   if (status!=1) IBISSignalU(unit2,status,1);

   mz_alloc1((unsigned char**)&inFile, nr, sizeof(double));
   mz_alloc1((unsigned char**)&outFile, clen, sizeof(double));

   for(i = 0; i < nc; i++)
   {
      int j, continueFlag, cpIndex;

      continueFlag = cpIndex = 0;
      for(j = 0; j < indcount; j++)
  	 if(i+1 == coeffcol[j] || (depcol2!=0 && coeffcol2[j] == i+1)
            || i+1 == indcol[j])
	 {
	    continueFlag = 1;
	    break;
	 }
      if(i+1 == rescol || i+1 == rescol2 || i+1 == depcol
         || i+1 == depcol2 || i+1 == concol)
	 continueFlag = 1;
      if(continueFlag) continue;

      status = IBISColumnSet(ibis1, "U_FORMAT", "DOUB", i+1);
      if(status != 1) IBISSignal(ibis1, status, 1);
      status = IBISColumnRead(ibis1, (char *)inFile, i+1, 1, nr);
      if(status != 1) IBISSignal(ibis1, status, 1);

      if(outlierCnt > 0)
      {
         for(j = 0; j < nr; j++)
         {
	    int k, outFlag;

	    outFlag = 0;
	    for(k = 0; k < outlierCnt; k++)
	       if(j == outliers[k])
               {
                  outFlag = 1;
		  break;
	       }

	    if(cpIndex > clen) printf("error accessing beyond array cpIndex: %d clen: %d \n", cpIndex, clen);
	    if(!outFlag)
            {
               outFile[cpIndex++] = inFile[j];
	       /*	       printf("infile: %f \n", inFile[j]);*/
	    }
         }
	 
	 /*for(j = 0; j < outlierCnt; j++) printf("%d\n", outliers[j]);
	 
	 printf("j: %d cpIndex: %d clen: %d\n", j, cpIndex, clen);
	 printf("last out: %f in: %f\n", outFile[cpIndex-2], inFile[j-2]);
	 */
      }
      else memcpy(outFile, inFile, clen*sizeof(double));

      status = IBISColumnWrite(ibis2, (char *)outFile, i+1, 1, clen);
      if(status!=1) IBISSignal(ibis2, status, 0);

   }

   free(inFile);
   free(outFile);

   if(indcol[0] != 0)
   {
      for(i = 0; i < indcount; i++)
      {
	 status = IBISColumnSet(ibis2, "U_FORMAT", "DOUB", indcol[i]);
	 if(status != 1) IBISSignal(ibis2, status, 1);
	 status = IBISColumnWrite(ibis2, (char *)(xpar[i]), indcol[i], 1, clen);
	 if(status != 1) IBISSignal(ibis2, status, 1);
      }
   }
   if(coeffcol[0]!=0)
   {
      for (i=0;i<indcount;i++)
      {
         status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",coeffcol[i]);
         if (status!=1) IBISSignal(ibis2,status,1);
         status = IBISColumnWrite(ibis2,(char *)(sout[i]),coeffcol[i],1,clen);
         if (status!=1) IBISSignal(ibis2,status,1);
      }
      mz_free2((unsigned char**)sout, indcount);
   }
   if(depcol2 != 0 && coeffcol2[0]!=0)
   {
      for (i=0;i<indcount;i++)
      {
         status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",coeffcol2[i]);
         if (status!=1) IBISSignal(ibis2,status,1);
         status = IBISColumnWrite(ibis2,(char *)(sout2[i]),coeffcol2[i],1,clen);
         if (status!=1) IBISSignal(ibis2,status,1);
      }
      mz_free2((unsigned char**)sout2, indcount);
   }
   if (rescol!=0)
   {
      status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",rescol);
      if (status!=1) IBISSignal(ibis2,status,1);
      status = IBISColumnWrite(ibis2,(char *)rout,rescol,1,clen);
      if (status!=1) IBISSignal(ibis2,status,1);
      free(rout);
   }
   if (rescol2!=0)
   {
      status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",rescol2);
      if (status!=1) IBISSignal(ibis2,status,1);
      status = IBISColumnWrite(ibis2,(char *)rout2,rescol2,1,clen);
      if (status!=1) IBISSignal(ibis2,status,1);
      free(rout2);
   }
   if(concol != 0)
   {
      status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",concol);
      if (status!=1) IBISSignal(ibis2,status,1);
      status = IBISColumnWrite(ibis2,(char *)concolv,concol,1,clen);
      if (status!=1) IBISSignal(ibis2,status,1);
   }
   if(depcol != 0)
   {
      status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",depcol);
      if (status!=1) IBISSignal(ibis2,status,1);
      status = IBISColumnWrite(ibis2,(char *)ypar,depcol,1,clen);
      if (status!=1) IBISSignal(ibis2,status,1);
   }
   if(depcol2 != 0)
   {
      status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",depcol2);
      if (status!=1) IBISSignal(ibis2,status,1);
      status = IBISColumnWrite(ibis2,(char *)ypar2,depcol2,1,clen);
      if (status!=1) IBISSignal(ibis2,status,1);
      free(ypar2);
   }

   free(outliers);
   mz_free2((unsigned char**)xpar, indcount);
   free(ypar);
   free(concolv);

   status = IBISFileClose(ibis1,0);
   if (status!=1) IBISSignal(ibis1,status,1);
   status = IBISFileClose(ibis2,0);
   if (status!=1) IBISSignal(ibis2,status,1);
}












