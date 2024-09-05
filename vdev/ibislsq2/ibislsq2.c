#include <math.h>
#include <stdio.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "carto/cartoVicarProtos.h"
#include "carto/cartoLsqUtils.h"
#include "carto/cartoMemUtils.h"

/************************************************************************/
/* program ibislsq2                                                      */
/************************************************************************/
/*  99-09 ...alz... initial version                     */
/************************************************************************/

void main44(void)
{
  int i,j,indcol[20],coeffcol[20],unit,indcount,coeffcount,coldef;
  int ibis,status,clen,depcol,rescol,concol,numlsq,ier;
  int dummy,noprint,uptr,lptr,igroup;
  float *concolv,groupnbr;
  double **xpar,**sout,*ypar,*rout,*clsq,*clsqdep,csol[20],eps;
  double sum;
           
  zifmessage("ibislsq2 version Thu Jan  3 2008");
   
  /* get the basic parameters */
   
  status = zvp("depcol",&depcol,&dummy);
  status = zvp("rescol",&rescol,&dummy);
  status = zvp("concol",&concol,&dummy);
  noprint = zvptst("noprint");
   
  zvparm("indcol",indcol,&indcount,&coldef,20,0);
  zvparm("coeffcol",coeffcol,&coeffcount,&coldef,20,0);
  if (coeffcount!=indcount&&coldef==0)
    zmabend("Count for parameter COEFFCOL wrong");
    
  /* read in data from the ibis interface file */

  status = zvunit(&unit,"inp",1, NULL);
  status = IBISFileOpen(unit,&ibis,"update",0,0,0,0);
  if (status!=1) IBISSignalU(unit,status,1);
  IBISFileGet(ibis,"nr",&clen,1,1,0);
   
  mz_alloc2((unsigned char ***)&xpar,indcount,clen,8);
  mz_alloc1((unsigned char **)&clsq,indcount*clen,8);
  mz_alloc1((unsigned char **)&clsqdep,clen,8);
  mz_alloc1((unsigned char **)&ypar,clen,8);
  mz_alloc1((unsigned char **)&concolv,clen,8);
   
  if (coeffcol[0]!=0) mz_alloc2((unsigned char ***)&sout,indcount,clen,8);
  if (rescol!=0) mz_alloc1((unsigned char **)&rout,clen,8);
   
  for (i=0;i<indcount;i++)
    {
      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",indcol[i]);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnRead(ibis,(char*)xpar[i],indcol[i],1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
    }
  status = IBISColumnSet(ibis,"U_FORMAT","DOUB",depcol);
  if (status!=1) IBISSignal(ibis,status,1);
  status = IBISColumnRead(ibis,(char*)ypar,depcol,1,clen);
  if (status!=1) IBISSignal(ibis,status,1);
   
  if (concol>0)
    {
      status = IBISColumnSet(ibis,"U_FORMAT","REAL",concol);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnRead(ibis,(char*)concolv,concol,1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
    }
  else for (i=0;i<clen;i++) concolv[i] = 1.0;
      
  /* do the least squares */
  /* could reduce storage need by saving solutions more compactly, and
     then free the input data, and unpack the solutions */
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
	for (j=lptr;j<uptr;j++) clsq[i*numlsq+j-lptr] = xpar[i][j];
      for (j=lptr;j<uptr;j++) clsqdep[j-lptr] = ypar[j];
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
      
      if (coeffcol[0]!=0) {
	for (i=0;i<indcount;i++)
	  for (j=lptr;j<uptr;j++)
	    if (ier==0) sout[i][j] = csol[i];
	    else sout[i][j] = -999.0;
      }
      if (rescol!=0) {
	for (j=lptr;j<uptr;j++)
	  if (ier==0)
	    {
	      sum = 0.0;
	      for (i=0;i<indcount;i++) sum += csol[i]*xpar[i][j];
	      rout[j] = ypar[j]-sum;
	    }
	  else rout[j] = 0.0;
      }
    }
     
  /* Output desired columns to the ibis interface file */
   
  if (coeffcol[0]!=0)
    for (i=0;i<indcount;i++)
      {
	status = IBISColumnSet(ibis,"U_FORMAT","DOUB",coeffcol[i]);
	if (status!=1) IBISSignal(ibis,status,1);
	status = IBISColumnWrite(ibis,(char*)sout[i],coeffcol[i],1,clen);
	if (status!=1) IBISSignal(ibis,status,1);
      }
  if (rescol!=0)
    {
      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",rescol);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnWrite(ibis,(char*)rout,rescol,1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
    }
   
  status = IBISFileClose(ibis,0);
  if (status!=1) IBISSignal(ibis,status,1);
  
  return;
}
