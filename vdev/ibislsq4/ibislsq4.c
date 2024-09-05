#include <math.h>
#include <stdio.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

//#include "carto/cartoVicarProtos.h"    /* cartlab only - zifmessage */
#include "carto/cartoLsqUtils.h"
#include "carto/cartoMemUtils.h"

// prototype
//int simple_lsq( double * a, double * r, int m, int n, double * x, double * s, double * i, double eps );
int simple_lsq( double *x, double *y, int m, double *sl, double *in, double *sigs, double *sigi, double eps );
/************************************************************************/
/* program ibislsq4  - derived from ibislsq2 - Jun 21, 2010             */
/************************************************************************/
/*  99-09 ...alz... initial version of ibislsq2                         */
/************************************************************************/

void main44(void)
{
    int i=0,j,indcol,unit;
    int ibis,status,clen,depcol,rescol,concol,isigcol,ssigcol,numlsq,ier;
    int slopecol,yintcol;
    int dummy,noprint,uptr,lptr,igroup;
    float *concolv,groupnbr;
    double *xpar,*ypar,*rout,*clsq,*clsqdep,eps;
    double sigslope,sigintcpt,*slopeval,*yintval,cslope,cintcpt;
    double *sigslopeval,*sigyintval;
    double sum;
        /* following changed from zifmessage */           
    zvmessage("ibislsq4 - Jul 02 2012 - (64-bit) - RJB"," ");
   
    /* get the basic parameters - 1 column each     */
   
    status = zvp("depcol",&depcol,&dummy);      //dependent variable column
    status = zvp("rescol",&rescol,&dummy);      //residuals column (deviation from fitted line)
    status = zvp("concol",&concol,&dummy);      //control column
    status = zvp("isigcol",&isigcol,&dummy);    //intercept sigma column
    status = zvp("ssigcol",&ssigcol,&dummy);    //slope sifma column 

    noprint = zvptst("noprint");
   
    status = zvp("indcol",&indcol,&dummy);         //independent variable column(s) and number of
    status = zvp("slopecol",&slopecol,&dummy);   //coefficient column(s) and number of (= to ind cols)  
    status = zvp("yintcol",&yintcol,&dummy);
//    if (coeffcount!=indcount&&coldef==0) {
//        zmabend("??E - Count for COEFFCOL must be same as INDCOL");
//    } 
    /* read in data from the ibis interface file */

    status = zvunit(&unit,"inp",1, NULL);
    status = IBISFileOpen(unit,&ibis,"update",0,0,0,0);
    if (status!=1) IBISSignalU(unit,status,1);
    IBISFileGet(ibis,"nr",&clen,1,1,0);
// allocate space for columns  


////printf ("before allocates\n");
    mz_alloc1((unsigned char **)&xpar,clen,8);        //since xpar is a handle
    mz_alloc1((unsigned char **)&clsq,clen,8);         //clsq is a pointer
    mz_alloc1((unsigned char **)&clsqdep,clen,8);               //clsqdep is a pointer
    mz_alloc1((unsigned char **)&ypar,clen,8);                  //ypar is a pointer
    mz_alloc1((unsigned char **)&concolv,clen,8);               //convolv is a pointer
   
//    if (coeffcol[0]!=0) mz_alloc2((unsigned char ***)&sout,indcount,clen,8);        //sout is a handle
    mz_alloc1((unsigned char **)&rout,clen,8);                       //rout is a pointer

    mz_alloc1((unsigned char **)&slopeval,clen,8);         //sigslope  is a pointer
    mz_alloc1((unsigned char **)&yintval,clen,8);         //sigma intercept is a pointer
    mz_alloc1((unsigned char **)&sigslopeval,clen,8);         //sigslope  is a pointer
    mz_alloc1((unsigned char **)&sigyintval,clen,8);         //sigma intercept is a pointer

// read independent column into xpar  (time?)
    status = IBISColumnSet(ibis,"U_FORMAT","DOUB",indcol);
    if (status!=1) IBISSignal(ibis,status,1);
    status = IBISColumnRead(ibis,(char*)xpar,indcol,1,clen);
    if (status!=1) IBISSignal(ibis,status,1);

// read dependent column into ypar (temps)
    status = IBISColumnSet(ibis,"U_FORMAT","DOUB",depcol);
    if (status!=1) IBISSignal(ibis,status,1);
    status = IBISColumnRead(ibis,(char*)ypar,depcol,1,clen);
    if (status!=1) IBISSignal(ibis,status,1);
   
// check for control column - if not then set control col to 1 in all rows
    if (concol>0) {
        status = IBISColumnSet(ibis,"U_FORMAT","REAL",concol);
        if (status!=1) IBISSignal(ibis,status,1);
        status = IBISColumnRead(ibis,(char*)concolv,concol,1,clen);
        if (status!=1) IBISSignal(ibis,status,1);
    } else {
        for (i=0;i<clen;i++) concolv[i] = 1.0;
    }  
    /* do the least squares */
    /* could reduce storage need by saving solutions more compactly, and
        then free the input data, and unpack the solutions                  */
    /* GO thru each group and fit each line                                 */
    /* upper pointer is lower row number                                    */
////    printf ("before lsq\n");
    uptr = 0;
    for (igroup=0;;igroup++) {
        lptr = uptr;
        if (lptr>=clen) break;                              //break if lower pointer > number of rows
        groupnbr = concolv[lptr];                           //get group number from control col
        for (uptr=lptr;uptr<=clen;uptr++)                   //range in control col
	        if (uptr==clen || concolv[uptr]!=groupnbr) break;   //break out if at end or not our group in control col
            numlsq = uptr-lptr;                             //number of points in lsq
//            for (i=0;i<indcount;i++)                        //go thru indep variables
	            for (j=lptr;j<uptr;j++) {
                    clsq[j-lptr] = xpar[j];
////                    printf ("%d: xpar[j] = %f\n",j,xpar[j]);
                }
                for (j=lptr;j<uptr;j++) {
                    clsqdep[j-lptr] = ypar[j];
////                    printf ("%d: ypar[j] = %f\n",j,ypar[j]);
                }
            eps = 1.e-7;
//            lsqfit(clsq,clsqdep,numlsq,indcount,csol,eps,&ier);
////            printf ("before simple_lsq\n");
                ier = simple_lsq(clsq,clsqdep,numlsq,&cslope,&cintcpt,&sigslope,&sigintcpt,eps);
////                printf ("after\n");
////                printf ("cslope = %f  cintcpt = %f\n",cslope,cintcpt); 
      /* printing if requested */
      
        if (!noprint) {
	        printf("  control   datcol      slope         sigslope     y-intercept      sigint\n");      //heading
//	        for (i=0;i<indcount;i++)
                if (ier==0) {
                    printf("%7.2f %7d %14.5f %14.5f %14.5f %14.5f\n",
			            groupnbr,indcol,cslope,sigslope,cintcpt,sigintcpt);
                } else {
                    printf("%7.2f %7d %14.05f\n",
			            groupnbr,indcol,-999.0);
                }
	        printf("\n");
	    }
      
      /* calculate the output data */
      
//        if (coeffcol[0]!=0) {
//	        for (i=0;i<indcount;i++)
	            for (j=lptr;j<uptr;j++) {
	                if (ier==0) {
                        slopeval[j] = cslope;
                        yintval[j] = cintcpt;
                        sigslopeval[j] = sigslope;
                        sigyintval[j] = sigintcpt;
////                        printf ("slopeval[j] = %f yintval[j] = %f\n",slopeval[j],yintval[j]);
	                } else {
                        slopeval[j] = -999.0;
                        yintval[j] = -999.0;
                        sigslopeval[j] = -999.0;
                        sigyintval[j] = -999.0;
                    }
                }
//        }
//        if (rescol!=0) {
            sum = 0.0;
	        for (j=lptr;j<uptr;j++) {
	            if (ier==0) {
	                sum = cslope*xpar[j] + cintcpt;
	                rout[j] = ypar[j]-sum;
////                    printf ("%d: rout[j] = %f sum = %f xpar[j] = %f ypar[j] = %f\n",j,rout[j],sum,xpar[j],ypar[j]);
	            } else {  
	                rout[j] = 0.0;
                }
            }
    }  //for (igroup=0;;igroup++)
    
////printf ("after loop\n"); 
  /* Output desired columns to the ibis interface file */
   
//    if (coeffcol[0]!=0)
//        for (i=0;i<indcount;i++) {
	        status = IBISColumnSet(ibis,"U_FORMAT","DOUB",slopecol);
	        if (status!=1) IBISSignal(ibis,status,1);
	        status = IBISColumnWrite(ibis,(char*)slopeval,slopecol,1,clen);
	        if (status!=1) IBISSignal(ibis,status,1);
            status = IBISColumnSet(ibis,"U_FORMAT","DOUB",yintcol);
            if (status!=1) IBISSignal(ibis,status,1);
            status = IBISColumnWrite(ibis,(char*)yintval,yintcol,1,clen);
            if (status!=1) IBISSignal(ibis,status,1);

            status = IBISColumnSet(ibis,"U_FORMAT","DOUB",ssigcol);
            if (status!=1) IBISSignal(ibis,status,1);
            status = IBISColumnWrite(ibis,(char*)sigslopeval,ssigcol,1,clen);
            if (status!=1) IBISSignal(ibis,status,1);
            status = IBISColumnSet(ibis,"U_FORMAT","DOUB",isigcol);
            if (status!=1) IBISSignal(ibis,status,1);
            status = IBISColumnWrite(ibis,(char*)sigyintval,isigcol,1,clen);
            if (status!=1) IBISSignal(ibis,status,1);

//        }
//    if (rescol!=0) {
        status = IBISColumnSet(ibis,"U_FORMAT","DOUB",rescol);
        if (status!=1) IBISSignal(ibis,status,1);
        status = IBISColumnWrite(ibis,(char*)rout,rescol,1,clen);
        if (status!=1) IBISSignal(ibis,status,1);
//    }
   
    status = IBISFileClose(ibis,0);
    if (status!=1) IBISSignal(ibis,status,1);
  
    return;
}
int simple_lsq( double *x, double *y , int m, double *sl, double *in, double *sigs, double *sigi, double eps )
{
int ier;
int j;
double sumx, sumy, sumx2, sumxy, sumd2;
double slope, yinter, div, dev, vary, dslope, dyinter;

ier = 0;
sumx = 0.0;
sumy = 0.0;
sumx2 = 0.0;
sumxy = 0.0;
sumd2 = 0.0;

for (j=0;j<m;j++) {
    sumx = sumx + x[j];
    sumy = sumy + y[j];
    sumx2 = sumx2 + x[j]*x[j];
    sumxy = sumxy + x[j]*y[j];
}

div = (double)m * sumx2 - sumx*sumx;
if (div == 0.0) {
    ier = -1;
    return ier;
}
//calc slope, y-intercept
slope = ((double)m * sumxy - sumx*sumy)/div;
yinter = (sumy*sumx2 - sumx*sumxy) /div;

////printf ("simple_lsq: slope = %f  y-intercept = %f\n",slope,yinter);
*sl = slope;
*in = yinter;
////printf ("after *sl = %f *in = %f\n",(float)*sl,(float)*in);
//cal slope and y-intercept std dev
for (j=0;j<m;j++) {
    dev = slope*x[j] + yinter - y[j];
    sumd2 = sumd2 + dev*dev;
}
vary = sumd2/((double)m - 1.0);
////printf("vary = %f\n",vary);
dslope = sqrt( ((double)m*vary)/div );
dyinter = sqrt( (sumx2*vary)/div);

////printf ("simple_lsq: Sig slope = %f  Sig y-intercept = %f\n",dslope,dyinter);
*sigs = dslope;
*sigi = dyinter;
////printf ("after *sigs = %f *sigi = %f\n",*sigs,*sigi);
return ier;
}
