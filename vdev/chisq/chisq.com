$!****************************************************************************
$!
$! Build proc for MIPL module chisq
$! VPACK Version 1.9, Friday, October 31, 2008, 17:34:06
$!
$! Execute by entering:		$ @chisq
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module chisq ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to chisq.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("chisq.imake") .nes. ""
$   then
$      vimake chisq
$      purge chisq.bld
$   else
$      if F$SEARCH("chisq.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake chisq
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @chisq.bld "STD"
$   else
$      @chisq.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create chisq.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack chisq.com -mixed -
	-s chisq.c -
	-i chisq.imake -
	-p chisq.pdf -
	-t tstchisq.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create chisq.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdio.h>
#include <string.h>

#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "cartoMemUtils.h"
#include "cartoVicarProtos.h"
#include "cartoLsqUtils.h"
#include "cartoSortUtils.h"

/*       FREQUENTLY USED VARIABLES IN THIS PROGRAM         */
/*=========================================================*/
/*                                                         */
/* concol - control column number (0 if no control column  */
/*            is specified)                                */
/* concolv - buffer containing control numbers from IBIS   */
/*           file                                          */
/* xpar - independent dataset                              */
/* ypar - dependent dataset                                */
/* ypar2 - depenedent dataset. optional may be             */
/*         uninitialized                                   */
/* clen - number of rows in output IBIS file               */
/* indcount - number of dependent variables in the         */
/*            equation (xpar)                              */
/* rout - residual data for ypar                           */
/* rout3 - residual data for ypar2                         */
/* thresh - threshold for determining outliers             */
/* outliers - buffer holding outlier indices               */
/* outlierCnt - number of outliers                         */
/* depcol2 - column number where ypar2 is in the IBIS file */
/*=========================================================*/

/*=========================================================*/
/* This function performs a global least squares fitting.  */
/* This function calls least squares fitting after         */
/* separating the data by its control number. the least    */
/* squares fitting.                                        */
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
      /* Find the indices of the data points with same */
      /* control numbers.  The control numbers are     */
      /* assumed to be grouped.                        */
      lptr = uptr;
      if (lptr>=clen) break;
      groupnbr = concolv[lptr];
      for (uptr=lptr;uptr<=clen;uptr++)
         if (uptr==clen||concolv[uptr]!=groupnbr) break;

      /* put dependent and independent data into buffs */
      numlsq = uptr-lptr;
      for (i=0;i<indcount;i++)
         for (j=lptr;j<uptr;j++) clsq[i*numlsq+j-lptr] = xpar[i][j];
      for (j=lptr;j<uptr;j++) clsqdep[j-lptr] = ypar[j];

      /* perform least squares fitting here */
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
/* This function throws out the outlier from xpar, ypar    */
/* ypar2, concolv (control column), and adjusts the clen   */
/* (number of rows in output IBIS file) and outlierCnt.    */
/* It also adds the outlier index into outliers buffer.    */
/*=========================================================*/
void throwout(int *outliers, int outlierCnt, int indcount, float *concolv,
              double *ypar, double *ypar2, double **xpar, int depcol2, int *clen)
{
   int i, j;

   printf("  **Point %d thrown out.  outlier cnt: %d**\n\n", outliers[outlierCnt-1], outlierCnt);

   --(*clen);
   for(i = outliers[outlierCnt-1]; i < *clen; i++)
   {
      concolv[i] = concolv[i+1];
      ypar[i] = ypar[i+1];
      for(j = 0; j < indcount; j++)
         xpar[j][i] = xpar[j][i+1];
      if(depcol2 > 0)
	 ypar2[i] = ypar2[i+1];
   }
}

/*=========================================================*/
/* This function calculates the residual error as sqrt of  */
/* x'*x'+y'+y' and stores the errors in rout3.             */
/*=========================================================*/
void getAbsResiduals(rout1, rout2, rout3, clen, thresh, depcol2)
   double thresh, *rout1, *rout2, *rout3;
   int clen, depcol2;
{
   int i;
   
   for(i = 0; i < clen; i++)
   {
      if(depcol2 != 0)
         rout3[i] = pow(rout1[i]*rout1[i] + rout2[i]*rout2[i], 0.5);
      else
	 rout3[i] = fabs(rout1[i]);
   }
}

/*=========================================================*/
/* This function gets the distances for datapoints in      */
/* pos and stores it into distance buffer.  The distance   */
/* is measured from origin.                                */
/*                                                         */
/* pos - buffer that has the datapoints (input)            */
/* distance - buffer that the distances will be stored     */
/*            into (output)                                */
/* indcount - number of independent variables in pos and   */
/*            origin. (input)                              */
/* controlCnt - number of entities in pos. (input)         */
/* origin - the point from where the distances are         */
/*          calculated.                                    */
/*=========================================================*/
void getDistances(double **pos, double *distance, int indcount, int controlCnt, double *origin)
{
   int i, j;

   for(i = 0; i < controlCnt; i++)
   {
      double sum;

      sum = 0.0;
      for(j = 0; j < indcount; j++)
	 sum += pow(origin[j]-pos[j][i], 2);
      distance[i] = sqrt(sum);
   }
}

/*=========================================================*/
/* This function finds 30 closest datapoints for each      */
/* global outlier and calculates the local least squares.  */
/* If the outlier is within a threshold after a local      */
/* fitting, it will not be thrown away.                    */
/*                                                         */
/*=========================================================*/
void doLocalFit(concol, concolv, xpar, ypar, ypar2, clen, indcount, rout, rout3, thresh, outliers, outlierCnt, depcol2)
   float *concolv;
   double **xpar, *ypar, *ypar2, *rout, *rout3, thresh;
   int concol, *clen, indcount, *outliers, *outlierCnt, depcol2;
{
   int *routSorted, i, controlCnt, *distSorted, outlierFound, reBuffer;
   double **controlInds, *controlDeps, *controlDeps2, *origin, locRout, locRout2, *distances;
   float control, oldControl;

   /* data used for input parameters into lsqfit */
   int ier;
   double sum, eps, csol[20], csol2[20];
   double *clsq, *tmpclsq, *clsqdep, *clsqdep2;

   mz_alloc1((unsigned char **)&clsq,indcount*(*clen),sizeof(double));
   mz_alloc1((unsigned char **)&tmpclsq, indcount*(*clen), sizeof(double));
   mz_alloc1((unsigned char **)&clsqdep,(*clen),sizeof(double));
   mz_alloc1((unsigned char **)&clsqdep2,(*clen),sizeof(double));
   /**********************************************/

   controlDeps = malloc((*clen)*sizeof(double));
   controlDeps2 = malloc((*clen)*sizeof(double));
   mz_alloc2((unsigned char ***)&controlInds,indcount,(*clen),sizeof(double));
   origin = malloc(indcount*sizeof(double));
   distances = malloc((*clen)*sizeof(double));
   distSorted = malloc((*clen)*sizeof(int));
   routSorted = malloc((*clen)*sizeof(int));

   /* sort residual from lowest to greatest */
   getSelectionSortIndices(rout3, routSorted, *clen, CART_DOUBLE);

   oldControl = -999; /* Set to an *unlikely control number.  */
                      /* -999 means the buffers have not been */
                      /* initialized.                         */
                      /* -998 means that the buffers were     */
                      /* initialized and no control numbers   */
                      /* are specified.                       */

   outlierFound = 0;
   reBuffer = 1;
   eps = 1.e-7;
   i = *clen-1;
   while(i >= 0 && rout3[routSorted[i]] > thresh)
   {
      int j, k;

      printf("point %d has a global residual: %f\n", routSorted[i], rout3[routSorted[i]]);

      if(concol != 0)
         control = concolv[routSorted[i]];

      /* Copy data with same control entities into buffer. */
      /* Data is copied into buffer iff there is a control */
      /* column and the previous control number does not   */
      /* equal the current control number OR current loop  */
      /* is the initial loop and there are no data inside  */
      /* the buffers.                                      */
      if((concol != 0 && control != oldControl) || (concol != 0 && reBuffer))
      {
         controlCnt = 0;
         oldControl = control;

	 //	 printf("rebuffered\n");
	 for(j = 0; j < *clen; j++)
	 {
	    if(concolv[j] == concolv[routSorted[i]])
	    {
	       for(k = 0; k < indcount; k++)
		  controlInds[k][controlCnt] = xpar[k][j];
	       controlDeps[controlCnt] = ypar[j];
	       if(depcol2 != 0) controlDeps2[controlCnt] = ypar2[j];

	       ++controlCnt;
	    }
	 }
      }
      else if((concol == 0 && oldControl != -999) || (concol == 0 && reBuffer))
      {
	//	 printf("rebuffered\n");
	 oldControl = -998; /* Specify that the buffers have been initialized */
	                    /* and there are no control numbers.              */

	 for(j = 0; j < indcount; j++)
	    memcpy(controlInds[j], xpar[j], (*clen)*sizeof(double));

	 memcpy(controlDeps, ypar, (*clen)*sizeof(double));
	 if(depcol2 != 0) memcpy(controlDeps2, ypar2, (*clen)*sizeof(double));

	 controlCnt = *clen;
      }
   
      /* get 30 closest points */
      if(controlCnt > 30)
      {
	 for(j = 0; j < indcount; j++)
	    origin[j] = xpar[j][routSorted[i]];

         getDistances(controlInds, distances, indcount, controlCnt, origin);
      
	 getSelectionSortIndices(distances, distSorted, controlCnt, CART_DOUBLE);
	 for(j = 0; j < 30; j++)
	 {
	   //	   printf("j: %d point picked: %d distance: %f\n", j, distSorted[j], distances[distSorted[j]]);
	    /* get independent data into buffer */
	    for(k = 0; k < indcount; k++)
	       clsq[k*30+j] = controlInds[k][distSorted[j]];
	 
	    /* get dependent data into buffer */
	    clsqdep[j] = controlDeps[distSorted[j]];
	    if(depcol2 != 0)
               clsqdep2[j] = controlDeps2[distSorted[j]];
	 }
      }
      else /* if less than 30 points then just do a memcpy */
      {
	 for(j = 0; j < indcount; j++)
	    memcpy(clsq+j*controlCnt, controlInds[j], controlCnt*sizeof(double));
	 memcpy(clsqdep, controlDeps, controlCnt*sizeof(double));
	 if(depcol2 != 0) memcpy(clsqdep2, controlDeps2, controlCnt*sizeof(double));
      }

      /* perform local least squares fit on 30 points (29 closest + itself) */
      memcpy(tmpclsq, clsq, indcount*(*clen)*sizeof(double));

      lsqfit(clsq, clsqdep, (controlCnt < 30) ? controlCnt:30, indcount, csol, eps, &ier);
      if(ier != 0) printf("CHISQ error in lsqfit - error code: %d controlCnt: %d\n", ier, controlCnt);
      memcpy(clsq, tmpclsq, indcount*(*clen)*sizeof(double));

      if(depcol2 != 0)
         lsqfit(clsq, clsqdep2, (controlCnt < 30) ? controlCnt:30, indcount, csol2, eps, &ier);
      if(ier != 0) printf("CHISQ error in lsqfit - error code: %d controlCnt: %d", ier, controlCnt);

      /* calculate the local residual */
      sum = 0.0;
      for(j=0; j<indcount; j++)
         sum += csol[j]*origin[j];

      locRout = ypar[routSorted[i]]-sum;
      if(depcol2 != 0)
      {
	 sum = 0.0;
	 for(j = 0; j < indcount; j++)
	    sum += csol2[j]*origin[j];

	 locRout2 = ypar2[routSorted[i]]-sum;

         locRout = pow(locRout*locRout + locRout2*locRout2, 0.5);
      }
      else locRout = abs(locRout);

      printf("\t       local residual : %f\n", locRout);

      if(locRout > thresh)
      {
         outliers[*outlierCnt] = routSorted[i];
	 ++(*outlierCnt);
	 outlierFound = reBuffer = 1;

         throwout(outliers, *outlierCnt, indcount, concolv, ypar, ypar2, xpar, depcol2, clen);
	 for(j = i-1; j >= 0; j--)
	    if(routSorted[j] > routSorted[i])
	       routSorted[j]--;
      }
      else
      {
         printf("  **Point %d is not thrown out.\n\n", routSorted[i]);
	 reBuffer = 0;
      }

      --i;
   }

   /* If no outliers were thrown away by local fitting then tell main44 that it's done. */
   if(!outlierFound)
   {
      outliers[*outlierCnt] = -1;
      ++(*outlierCnt);
   }

   free(clsq);
   free(clsqdep);
   free(clsqdep2);
   free(controlDeps);
   free(controlDeps2);
   mz_free2((unsigned char**)controlInds, indcount);
   free(origin);
   free(distances);
   free(routSorted);
   free(distSorted);

   printf("local fit done.\n");
}

/*=========================================================*/
void main44(void)
{
   int i,indcol[20],coeffcol[20],unit,unit2,indcount,coeffcount,coldef;
   int coeffcol2[20],coeffcount2, coldef2;
   int ibis1,ibis2,status,clen,nc,nr,depcol,rescol,concol;
   int depcol2,rescol2, *outliers;
   int dummy,noprint, outlierCnt, cnt, def;
   double thresh;
   float *concolv;
   double **xpar,**sout,*ypar,*rout;
   double **sout2,*ypar2,*rout2,*rout3;
   double *inFile, *outFile;
   int attempt;

   zifmessage("chisq version 24-oct-02");
   
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
   /* open input files */
   status = zvunit(&unit,"inp",1,0);
   status = IBISFileOpen(unit,&ibis1,"read",0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis1,"nr",&nr,1,1,0);
   IBISFileGet(ibis1,"nc",&nc,1,1,0);
   clen=nr;

   /* allocate memories */
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
   if(rescol2 != 0)
      mz_alloc1((unsigned char**)&rout2, clen, 8);
   mz_alloc1((unsigned char**)&rout3, clen, 8);
   
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

   /* perform least squares */
   printf("======================\n");
   printf("ATTEMPT: %d\n", (attempt = 1));
   doLstSq(concolv, xpar, ypar, clen, indcount, indcol, coeffcol, sout, noprint, rescol, rout);
   if(depcol2 != 0)
      doLstSq(concolv, xpar, ypar2, clen, indcount, indcol, coeffcol2, sout2, noprint, rescol2, rout2);

   /* find outlier */
   if(thresh > .0)
      getAbsResiduals(rout, rout2, rout3, clen, thresh, depcol2);

   if(thresh > .0)
      doLocalFit(concol, concolv, xpar, ypar, ypar2, &clen, indcount, rout, rout3, thresh, outliers, &outlierCnt, depcol2);

   while(thresh > .0 && outliers[outlierCnt-1] != -1)
   {
      /* perform least squares again */
      printf("======================\n");
      printf("ATTEMPT: %d\n", ++attempt);
      doLstSq(concolv, xpar, ypar, clen, indcount, indcol, coeffcol, sout, noprint, rescol, rout);
      if(depcol2 != 0)
      {
         doLstSq(concolv, xpar, ypar2, clen, indcount, indcol, coeffcol2, sout2, noprint, rescol2, rout2);
         getAbsResiduals(rout, rout2, rout3, clen, thresh, depcol2);
      }
      doLocalFit(concol, concolv, xpar, ypar, ypar2, &clen, indcount, rout, rout3, thresh, outliers, &outlierCnt, depcol2);
   }
   printf("  All points are under error threshold.**\n");

   /* Outlier indices are not what they were because of array shifting after
      throwing out each outlier.  We need to calculate the original index
      of the outliers                                                        */
   {
      int oi, oj;

      for(oi = outlierCnt-2; oi > 0; oi--)
      {
	 int actualIndex = outliers[oi];

	 for(oj = oi-1; oj >= 0; oj--)
	 {

	    if(actualIndex >= outliers[oj]) 
            {
               actualIndex++;
	    }
	 }
	 outliers[oi] = actualIndex;
      }
   }

   /* quick check */
   if(outlierCnt > 0 && (outlierCnt-1)+clen != nr)
      zmabend("Some entities were lost.\n");

   /* Output desired columns to the ibis interface file */
   /* Open output file */
   zvselpi(0);

   status = zvunit(&unit2,"out",1,0);
   if (status!=1) IBISSignalU(unit2,status,1);
   status = IBISFileUnit(unit2, &ibis2, "write", nc, clen, 0, 0);
   if (status!=1) IBISSignalU(unit2,status,1);
   status = IBISFileSet(ibis2, "fmt_default", "doub", 1);
   if (status!=1) IBISSignalU(unit2,status,1);
   status = IBISFileUnitOpen(ibis2);
   if (status!=1) IBISSignalU(unit2,status,1);

   mz_alloc1((unsigned char**)&inFile, nr, sizeof(double));
   mz_alloc1((unsigned char**)&outFile, clen, sizeof(double));

   /* output to file - inside this loop, columns that are
      unrelated to the least squares fitting are copied
      just to keep the data in the output file same as the
      data in the input file                               */
   for(i = 0; i < nc; i++)
   {
      int j, continueFlag, cpIndex;

      /* If i+1 equals a column that we handle later then skip for now */
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

      /* if there are no outliers then we can just copy the entire
         column... but if there are outliers then we need to not
         copy those                                               */
      if(outlierCnt > 0)
      {
         for(j = 0; j < nr; j++)
         {
	    int k, outFlag;

	    outFlag = 0;

	    /* check to see if current row is marked as an outlier */
	    for(k = 0; k < outlierCnt; k++)
	       if(j == outliers[k])
               {
                  outFlag = 1;
		  break;
	       }

	    /* check to make sure that the cpIndex (index of output
               column) is not greater than the length of output column */
	    if(cpIndex > clen) printf("error accessing beyond array cpIndex: %d clen: %d \n", cpIndex, clen);

	    /* if it's not an outlier then copy to output column */
	    if(!outFlag)
            {
               outFile[cpIndex++] = inFile[j];
	    }
         }
      }
      else memcpy(outFile, inFile, clen*sizeof(double));

      status = IBISColumnWrite(ibis2, (char *)outFile, i+1, 1, clen);
      if(status!=1) IBISSignal(ibis2, status, 0);

   }

   free(inFile);
   free(outFile);

   /* now copy over all the columns that were necessary in the least
      squares fitting                                                */
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












$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create chisq.imake
#define  PROGRAM   chisq

#define MODULE_LIST chisq.c

#define MAIN_LANG_C
#define R2LIB 

/* Comment this out before delivery.*/
#define DEBUG


#define USES_ANSI_C

#define LIB_CARTO
#define LIB_P2SUB
#define LIB_TAE
#define LIB_RTL
$ Return
$!#############################################################################
$PDF_File:
$ create chisq.pdf
PROCESS       HELP=*
PARM INP       TYPE=(STRING)
PARM OUT       TYPE=(STRING)
PARM INDCOL    TYPE=INTEGER COUNT=1:20
PARM DEPCOL    TYPE=INTEGER COUNT=1
PARM DEPCOL2   TYP=INTEGER COUNT=1 DEFAULT=0
PARM COEFFCOL  TYPE=INTEGER COUNT=1:20 DEFAULT=0
PARM COEFFCOL2 TYPE=INTEGER COUNT=1:20 DEFAULT=0
PARM RESCOL    TYPE=INTEGER COUNT=1 DEFAULT=0
PARM RESCOL2   TYPE=INTEGER COUNT=1 DEFAULT=0
PARM CONCOL    TYPE=INTEGER DEFAULT=0
PARM NOPRINT   TYPE=KEYWORD COUNT=0:1 VALID=(--,NOPRINT) DEFAULT=--
PARM THRESH    TYPE=REAL COUNT=1 DEFAULT=0.0
END-PROC
.TITLE
VICAR/IBIS Program "chisq"
.HELP
PURPOSE

   "chisq" performs least squares fits on data in an IBIS interface
(tabular) file.  The solutions and/or residuals can be placed in 
specified columns of the file.  The solutions can also be output
to the terminal.  Multiple fits can be done on different parts of
one file.  A threshold can also be specified so that the program
will repeatedly perform least sqares fitting while throwing out
outliers until all the residuals are under the defined  threshold.  
It first performs a "global" least squares fitting by using all
a grouped dataset.  (Datasets are grouped by the values in CONCOL.)
For all datapoints that have residuals greater than the threshold,
it will obtain 30 closest datapoints and perform a "local" least
squares fitting.  If the local residual is still greater than
the threshold then the datapoint is thrown away.  If not, it is
kept.

Note that if only x' is specified, the outlier will
be determined by the data point's residual value.  If both x' and
y' are specified then the outlier will be determined by
square root of (residual(x')^2 + residual(y')^2).

EXECUTION

     chisq INP=DATA.INT OUT=DATAOUT INDCOL=(1,2,3) DEPCOL=4
             CONCOL=7 RESCOL=8  COEFFCOL=(21,22,23) DEPCOL2=5
             RESCOL2=8 COEFFCOL2=(24,25,26) 'NOPRINT THRESH=2.0

    This example shows the use of all of the parameters.  The input
file, DATA.INT, is an IBIS interface file and the output file
is also an IBIS interface file.  The data for the independent
variables are in columns (1,2,3), and the data for the dependent 
variables are in column 4 and 5.  The control column 7 is used for
multiple fits to be done in one run.  The least square fits are done 
on sets of rows; a new set is started whenever the value in the control 
column changes.  If no control column is specified then one fit
is done on the whole file.  The COEFFCOL and RESCOL parameters specify
in which columns, of the input file, the results will be put.  If either
or both are not specified then they will not be output.  The COEFFCOL2
and RESCOL2 correspond to the DEPCOL2 data.  There must be
as many coefficient columns as there are independent columns and these
match the sequence of the independent variable columns.  The
coefficients of the solution are placed in columns 21,22,23.  The
residual column can be used to easily calculate the deviation of the
data points from the fitted line. Normally the solution for each set
is printed to the terminal, but this can be turned off with the
'NOPRINT keyword.  

    The length of each set should, of course, be longer than the
number of independent variables (columns).  If it is not then the
least squares fit will not be called and values of -999.0 will be
put out for the solution.  If some columns of the independent data
are dependent then the error MATRIX RANK TOO SMALL be be printed,
and -999.0's will be put out for the solution.  If there is no
solution then zeros will be put out for the residuals.

EXAMPLES

   Suppose that columns 1 and 2 contain points (x,y) in a plane 
   and  column  7 contains a function  f(x,y).   The  following 
   sequence  will perform a quadratic least squares fit  h(x,y) 
   and place the residuals in column 8.

   mf INP=A FUNCTION=("C3=C1*C1","C4=C2*C2","C5=C1*C2","C6=1")
   chisq INP=A OUT=B INDCOL=(1,2,3,4,5,6) DEPCOL=7 RESCOL=8

   Suppose that you want the program to automatically take out
   outliers one at a time and reperform least squares fitting.
   To repeat until all residuals are below 2.0:

   chisq INP=A OUT=B INDCOL=(1,2,3,4,5,6) DEPCOL=7 RESCOL=8
      THRESH=2.0

   Suppose now that columns 1, 2, and 3 represent the independent
   variables, columns 4 and 5 represent x' and y' respectively,
   column 7 represents the control column, 8 and 9 represents
   the residual columns for x' and y' respectively, columns 10,
   11, 12 represents the coefficients to the solution to x',
   and columns 16, 17, 18 represents the coefficients to
   y'.  Suppose also that we would like the solution to have the
   maximum residual for any data point to be below 2.0 and
   we want to not print the solution to the screen.  The
   following command performs this task:

   chisq inp=a out=b indcol=(1,2,3) depcol=4 depcol2=5 +
      colcol=7 rescol=8 rescol2=9 coeffcol=(10,11,12) +
      coeffcol2=(16,17,18) thresh=2.0 'NOPRINT

RESTRICTIONS

The maximum number of independent columns (variables) is 20.
DEPCOL, COEFFCOL, and RESCOL corresponds to each other and
DEPCOL2, COEFFCOL2, and RESCOL2 corresponds to each other.
You can not mix them together... for example, you can not
have DEPCOL correspond to RESCOL2.

Original Programmers: P. Kim, A. L. Zobrist 8 May 2008
Current Cognizant Programmer: P. Kim, 8 May 2008

.LEVEL1
.VARIABLE INP
 Input IBIS interface file
.VARIABLE OUT
 Output IBIS interface file
.VARIABLE INDCOL
 Independent variable columns
.VARIABLE DEPCOL
 Dependent variable column
.VARIABLE DEPCOL2
 Seconde dependent variable column
.VARIABLE COEFFCOL
 Optional columns to place
 coefficients of the solution
.VARIABLE COEFFCOL2
 Second optional columns to
 place coefficients of the solution
.VARIABLE RESCOL
 Residuals column
.VARIABLE RESCOL2
 Second residuals column
.VARIABLE CONCOL
 Control column
.VARIABLE NOPRINT
 Keyword to suppress printout
.VARIABLE THRESH
 Threshold value for maximum
 residual value.

.LEVEL2
.VARIABLE INP
   INP=file           Input IBIS interface file

.VARIABLE OUT
   OUT=file           Output IBIS interface file containing the
                      solution.

.VARIABLE INDCOL
   INDCOL=(N1,...,Nk) The   integers   N1,...,Nk  specify   the 
                      columns  which contain the data  for  the 
                      independent variables.  Each row contains 
                      one data point.

.VARIABLE DEPCOL
   DEPCOL=M1          The integer M1  specifies   the columns
                      which contain the data  for  the 
                      dependent variable.  Each row contains 
                      one data point.

.VARIABLE DEPCOL2
   DEPCOL=M2          The integer M2 specifies the second column
                      which contains the data for the second
                      dependent variable set.  Each row contains
                      one data point.

.VARIABLE COEFFCOL
   COEFFCOL=(N1,...,Nk) The   integers   N1,...,Nk  specify   the 
                      columns  which will receive the coefficients
                      of the solution for use in applying to
                      other operations in the IBIS file (for example
                      a slope or trend of some data points can be
                      found and the slope can be applied to other
                      data).  If this parameter is omitted,  
		      then no residuals are stored.
		      
		      Note that there is a lot of repetition of the
		      coefficients in the columns, but this allows
		      the coefficients to be used in a row-oriented
		      arithmetic operation by routine MF.

.VARIABLE COEFFCOL2
   COEFFCOL=(I1,...,IK) The integers I1,...,Ik specify the
                      columns which will receive the coefficients
                      of the solution corresponding to the
                      DEPCOL2 variable.  If this parameter is
                      omitted then no residuals for the second
                      solution will be stored.

.VARIABLE RESCOL
   RESCOL=R1          The  integer R1 specifies the  column which 
                      will  receive the residuals corresponding 
		      to the dependent variables for each data
		      point.  If this parameter is omitted,  
		      then no residuals are stored.

.VARIABLE RESCOL2
   RESCOL2=R2         The integer R2 specifies the column which
                      will receive the residuals corresponding
                      to the dependent variables for each data
                      point.  It corresponds to COEFFCOL2 and
                      DEPCOL2.  If this parameter is omitted,
                      then no residuals for DEPCOL2 are stored.

.VARIABLE CONCOL
   CONCOL=C           The integer C specifies a control column 
                      which produces a separate fit for each 
                      group of identical numbers in the control 
                      column.  If this keyword is omitted, then 
                      the whole file is DGELGd in one least 
                      squares fit.

.VARIABLE NOPRINT
    'NOPRINT	      Keyword to suppress printout of solution.

.VARIABLE THRESH
    THRESH=T          The real value T specifies the threshold
                      value which specifies the maximum allowable
                      residual value.  If the THRESH value is
                      specified, the program continues to perform
                      least squares fitting until all the
                      residuals are below this value.
.END











$ Return
$!#############################################################################
$Test_File:
$ create tstchisq.pdf
procedure
refgbl $echo
refgbl $autousage
parm mean real def=0.0
parm sdev real def=1.0
parm seed real def=9.0
body
let $autousage="none"
!let _onfail="continue"
let $echo="yes"

!  simple case

ibis-gen xxa nc=6 nr=3 datacols=(1,2,3) +
    data=(0.0,1.0,3.4,1.0,1.0,4.2,2.0,1.0,4.4)
 
chisq inp=xxa out=xxa1 indcol=(1,2) depcol=3 coeffcol=(4,5) rescol=6 +
    thresh=2.5

ibis-l xxa1


! The creation of IBIS files using mf3 was commented out due to
! errors in mf3 at the time of develpment.  Instead, the xxb
! file was manually put into /home/pkim/vdev_test_data directory
! on minaret. - p. kim (8 May 2008)


!	The test is a least squares fit of a linear 2-D vector field.
!		x' =  2.5 + 0.65x - 0.30y + Nx
!		y' = -1.3 + 1.20x + 0.15y + Ny 
!	   where Nx and Ny are gaussian noise with a sigma of 0.01. 
!	   The indepedent x and y are set up to be an 8 by 8 grid 
!		of unit spacing. 
!
!  The columns of the IBIS interface file are as follows: 
!
!		Input columns          |         Output columns 
!   1    2   3    4   5      6     7       8       9        10       11   
!  1's   x   y    x'  y'   Index Cntrl    Res x'  Res y'   Sol x'   Sol y'
!
!
!  ibis-gen xxb NC=19 NR=128
!  mf3 xxb FUNCTION=("C1=(sin(3.7*index**2)+1)/2",  +
!           "C2=(sin(5.3*(index+&seed)**2)+1)/2",  +
!           "C14=(sqrt(-2*alog(C1))*sin(2*(3.1415926)*C2))", +
!           "C14=&mean + &sdev*C14")
!  let seed = 8.0
!  mf3 xxb FUNCTION=("C1=(sin(3.7*index**2)+1)/2",  +
!           "C2=(sin(5.3*(index+&seed)**2)+1)/2",  +
!           "C15=(sqrt(-2*alog(C1))*sin(2*(3.1415926)*c2))",  +
!           "C15=&mean + &sdev*c15")
!  mf3 xxb FUNCTION=("C6=INDEX-1", "C7=INT(C6/64)", "C1=1",  +
!           "C2=MOD(C6,8)", "C3=INT(C6/8)"  +
!           "C4 =  2.5*C1 + 0.65*C2 - 0.30*C3 + C14",  +
!           "C5 = -1.3*C1 + 1.20*C2 + 0.15*C3 + C15" )
!  chisq inp=xxb out=xxb1 INDCOL=(1,2,3)  DEPCOL=4  CONCOL=7 +
!	  RESCOL=8 coeffcol=(10,11,12) THRESH=2
!  chisq inp=xxb1 out=xxb2 INDCOL=(1,2,3)  DEPCOL=5  CONCOL=7 +
!	  RESCOL=9

chisq inp=../test_data/xxb out=xxb1 indcol=(1,2,3) depcol=4 depcol2=5 +
   concol=7 rescol=8 rescol2=9 coeffcol=(10,11,12) +
   coeffcol2=(16,17,18) thresh=3.0 'noprint

END-PROC








$ Return
$!#############################################################################
