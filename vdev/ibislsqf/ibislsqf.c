#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdio.h>

#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "carto/cartoLsqUtils.h"
#include "carto/cartoMemUtils.h"
#include "carto/cartoVicarProtos.h"

/************************************************************************/
/* program ibislsqf                                                      */
/************************************************************************/
/*  april 05 ...alz... initial version                     */
/*  Fri Dec 28 2007 wlb switched to USES_ANSI_C AND LIB_CARTO; misc cleanup */
/************************************************************************/

/* the following accessible to functions */
double **xpar,*ypar,*clsq,*clsqdep,tguess[100],csol[100],*rout;
int n_ind_var,n_data_pt,solcol[5],n_guess,rpctype,rpc_linsmp;
char func_name[21];


/* the RPF routines ******************************************/

void rpctran(buf1,buf2,offset)
   double *buf1,*buf2;
   int offset;
{
   if (rpctype==0)       /* type RPC00A */
      {
      buf1[0] = buf2[offset];
      buf1[1] = buf2[offset+1];
      buf1[2] = buf2[offset+2];
      buf1[3] = buf2[offset+4];
      buf1[4] = buf2[offset+8];
      buf1[5] = buf2[offset+9];
      buf1[6] = buf2[offset+11];
      buf1[7] = buf2[offset+12];
      buf1[8] = buf2[offset+14];
      buf1[9] = buf2[offset+15];
      }
   else                   /* type RPC00B */
      {
      buf1[0] = buf2[offset];
      buf1[1] = buf2[offset+1];
      buf1[2] = buf2[offset+2];
      buf1[3] = buf2[offset+4];
      buf1[4] = buf2[offset+7];
      buf1[5] = buf2[offset+8];
      buf1[6] = buf2[offset+11];
      buf1[7] = buf2[offset+12];
      buf1[8] = buf2[offset+14];
      buf1[9] = buf2[offset+15];
      }
   return;
}

void rpc11tran(buf1,buf2,offset)
   double *buf1,*buf2;
   int offset;
{
   if (rpctype==0)       /* type RPC00A */
      {
      buf1[0] = buf2[offset];
      buf1[1] = buf2[offset+1];
      buf1[2] = buf2[offset+2];
      buf1[3] = buf2[offset+4];
      buf1[4] = buf2[offset+8];
      buf1[5] = buf2[offset+9];
      buf1[6] = buf2[offset+11];
      buf1[7] = buf2[offset+12];
      buf1[8] = buf2[offset+14];
      buf1[9] = buf2[offset+15];
      buf1[10] = buf2[offset+3];
      }
   else                   /* type RPC00B */
      {
      buf1[0] = buf2[offset];
      buf1[1] = buf2[offset+1];
      buf1[2] = buf2[offset+2];
      buf1[3] = buf2[offset+4];
      buf1[4] = buf2[offset+7];
      buf1[5] = buf2[offset+8];
      buf1[6] = buf2[offset+11];
      buf1[7] = buf2[offset+12];
      buf1[8] = buf2[offset+14];
      buf1[9] = buf2[offset+15];
      buf1[10] = buf2[offset+3];
      }
   return;
}

void rpcztran(buf1,buf2,offset)
   double *buf1,*buf2;
   int offset;
{
   if (rpctype==0)       /* type RPC00A */
      {
      buf1[0] = buf2[offset];
      buf1[1] = buf2[offset+3];
      buf1[2] = buf2[offset+5];
      buf1[3] = buf2[offset+6];
      buf1[4] = buf2[offset+7];
      buf1[5] = buf2[offset+10];
      buf1[6] = buf2[offset+13];
      buf1[7] = buf2[offset+16];
      buf1[8] = buf2[offset+17];
      buf1[9] = buf2[offset+18];
      buf1[10] = buf2[offset+19];
      }
   else                   /* type RPC00B */
      {
      buf1[0] = buf2[offset];
      buf1[1] = buf2[offset+3];
      buf1[2] = buf2[offset+5];
      buf1[3] = buf2[offset+6];
      buf1[4] = buf2[offset+9];
      buf1[5] = buf2[offset+10];
      buf1[6] = buf2[offset+13];
      buf1[7] = buf2[offset+16];
      buf1[8] = buf2[offset+17];
      buf1[9] = buf2[offset+18];
      buf1[10] = buf2[offset+19];
      }
   return;
}

void rpc2tran(buf1,buf2,offset)
   double *buf1,*buf2;
   int offset;
{
   if (rpctype==0)       /* type RPC00A */
      {
      buf1[0] = buf2[offset];
      buf1[1] = buf2[offset+3];
      }
   else                   /* type RPC00B */
      {
      buf1[0] = buf2[offset];   /* codes are same for this case */
      buf1[1] = buf2[offset+3];
      }
   return;
}

void rpc1tran(buf1,buf2,offset)
   double *buf1,*buf2;
   int offset;
{
   if (rpctype==0)       /* type RPC00A */
      {
      buf1[0] = buf2[offset+3];
      }
   else                   /* type RPC00B */
      {
      buf1[0] = buf2[offset+3];   /* codes are same for this case */
      }
   return;
}

void rpctranb(buf1,buf2,offset)
   double *buf1,*buf2;
   int offset;
{
   if (rpctype==0)       /* type RPC00A */
      {
      buf1[offset] = buf2[0];
      buf1[offset+1] = buf2[1];
      buf1[offset+2] = buf2[2];
      buf1[offset+4] = buf2[3];
      buf1[offset+8] = buf2[4];
      buf1[offset+9] = buf2[5];
      buf1[offset+11] = buf2[6];
      buf1[offset+12] = buf2[7];
      buf1[offset+14] = buf2[8];
      buf1[offset+15] = buf2[9];
      }
   else                   /* type RPC00B */
      {
      buf1[offset] = buf2[0];
      buf1[offset+1] = buf2[1];
      buf1[offset+2] = buf2[2];
      buf1[offset+4] = buf2[3];
      buf1[offset+7] = buf2[4];
      buf1[offset+8] = buf2[5];
      buf1[offset+11] = buf2[6];
      buf1[offset+12] = buf2[7];
      buf1[offset+14] = buf2[8];
      buf1[offset+15] = buf2[9];
      }
   return;
}

void rpc11tranb(buf1,buf2,offset)
   double *buf1,*buf2;
   int offset;
{
   if (rpctype==0)       /* type RPC00A */
      {
      buf1[offset] = buf2[0];
      buf1[offset+1] = buf2[1];
      buf1[offset+2] = buf2[2];
      buf1[offset+4] = buf2[3];
      buf1[offset+8] = buf2[4];
      buf1[offset+9] = buf2[5];
      buf1[offset+11] = buf2[6];
      buf1[offset+12] = buf2[7];
      buf1[offset+14] = buf2[8];
      buf1[offset+15] = buf2[9];
      buf1[offset+3] = buf2[10];
      }
   else                   /* type RPC00B */
      {
      buf1[offset] = buf2[0];
      buf1[offset+1] = buf2[1];
      buf1[offset+2] = buf2[2];
      buf1[offset+4] = buf2[3];
      buf1[offset+7] = buf2[4];
      buf1[offset+8] = buf2[5];
      buf1[offset+11] = buf2[6];
      buf1[offset+12] = buf2[7];
      buf1[offset+14] = buf2[8];
      buf1[offset+15] = buf2[9];
      buf1[offset+3] = buf2[10];
      }
   return;
}

void rpcztranb(buf1,buf2,offset)
   double *buf1,*buf2;
   int offset;
{
   if (rpctype==0)       /* type RPC00A */
      {
      buf1[offset] = buf2[0];
      buf1[offset+3] = buf2[1];
      buf1[offset+5] = buf2[2];
      buf1[offset+6] = buf2[3];
      buf1[offset+7] = buf2[4];
      buf1[offset+10] = buf2[5];
      buf1[offset+13] = buf2[6];
      buf1[offset+16] = buf2[7];
      buf1[offset+17] = buf2[8];
      buf1[offset+18] = buf2[9];
      buf1[offset+19] = buf2[10];
      }
   else                   /* type RPC00B */
      {
      buf1[offset] = buf2[0];
      buf1[offset+3] = buf2[1];
      buf1[offset+5] = buf2[2];
      buf1[offset+6] = buf2[3];
      buf1[offset+9] = buf2[4];
      buf1[offset+10] = buf2[5];
      buf1[offset+13] = buf2[6];
      buf1[offset+16] = buf2[7];
      buf1[offset+17] = buf2[8];
      buf1[offset+18] = buf2[9];
      buf1[offset+19] = buf2[10];
      }
   return;
}

void rpc2tranb(buf1,buf2,offset)
   double *buf1,*buf2;
   int offset;
{
   if (rpctype==0)       /* type RPC00A */
      {
      buf1[offset] = buf2[0];
      buf1[offset+3] = buf2[1];
      }
   else                   /* type RPC00B */
      {
      buf1[offset] = buf2[0];
      buf1[offset+3] = buf2[1];
      }
   return;
}

void rpc1tranb(buf1,buf2,offset)
   double *buf1,*buf2;
   int offset;
{
   if (rpctype==0)       /* type RPC00A */
      {
      buf1[offset+3] = buf2[0];
      }
   else                   /* type RPC00B */
      {
      buf1[offset+3] = buf2[0];
      }
   return;
}

int rpc_f10()
{
  double rpcn[20],rpcd[20],l,p,h,l2,p2,h2,l3,p3,h3,numer,denom;
  int strcmp();
  size_t i;

  for (i=0;i<20;i++) rpcn[i] = tguess[i+rpc_linsmp*40];
  for (i=0;i<20;i++) rpcd[i] = tguess[i+20+rpc_linsmp*40];

  for (i=0;i<n_data_pt;i++)
    {
    l = xpar[0][i];
    p = xpar[1][i];
    h = xpar[2][i];
    l2 = l*l; l3 = l2*l;
    p2 = p*p; p3 = p2*p;
    h2 = h*h; h3 = h2*h;
    
    if (rpctype==0)       /* type RPC00A */
       {
       numer = rpcn[3]*h+
       rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*p*l*h+
       rpcn[10]*h2+
       rpcn[13]*l2*h+
       rpcn[16]*p2*h+rpcn[17]*l*h2+
       rpcn[18]*p*h2+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*p*l*h+
       rpcd[8]*l2+rpcd[9]*p2+rpcd[10]*h2+rpcd[11]*l3+
       rpcd[12]*l2*p+rpcd[13]*l2*h+rpcd[14]*l*p2+
       rpcd[15]*p3+rpcd[16]*p2*h+rpcd[17]*l*h2+
       rpcd[18]*p*h2+rpcd[19]*h3;
       clsq[i] = 1.0;
       clsq[n_data_pt+i] = l;
       clsq[n_data_pt*2+i] = p;
       clsq[n_data_pt*3+i] = l*p;
       clsq[n_data_pt*4+i] = l2;
       clsq[n_data_pt*5+i] = p2;
       clsq[n_data_pt*6+i] = l3;
       clsq[n_data_pt*7+i] = l2*p;
       clsq[n_data_pt*8+i] = l*p2;
       clsq[n_data_pt*9+i] = p3;
       }
    else                   /* type RPC00B */
       {
       numer = rpcn[3]*h+
       rpcn[5]*l*h+rpcn[6]*p*h+
       rpcn[9]*h2+rpcn[10]*p*l*h+
       rpcn[13]*l*h2+
       rpcn[16]*p*h2+rpcn[17]*l2*h+
       rpcn[18]*p2*h+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*l2+
       rpcd[8]*p2+rpcd[9]*h2+rpcd[10]*p*l*h+rpcd[11]*l3+
       rpcd[12]*l*p2+rpcd[13]*l*h2+rpcd[14]*l2*p+
       rpcd[15]*p3+rpcd[16]*p*h2+rpcd[17]*l2*h+
       rpcd[18]*p2*h+rpcd[19]*h3;
       clsq[i] = 1.0;
       clsq[n_data_pt+i] = l;
       clsq[n_data_pt*2+i] = p;
       clsq[n_data_pt*3+i] = l*p;
       clsq[n_data_pt*4+i] = l2;
       clsq[n_data_pt*5+i] = p2;
       clsq[n_data_pt*6+i] = l3;
       clsq[n_data_pt*7+i] = l*p2;
       clsq[n_data_pt*8+i] = l2*p;
       clsq[n_data_pt*9+i] = p3;
       }

    clsqdep[i] = ypar[i]*denom-numer;
    }
  
  return 1;
}

int rpc_f20()
{
  double rpcn[20],rpcd[20],l,p,h,l2,p2,h2,l3,p3,h3,denom;
  int strcmp();
  size_t i;

  for (i=0;i<20;i++) rpcn[i] = tguess[i+rpc_linsmp*40];
  for (i=0;i<20;i++) rpcd[i] = tguess[i+20+rpc_linsmp*40];

  for (i=0;i<n_data_pt;i++)
    {
    l = xpar[0][i];
    p = xpar[1][i];
    h = xpar[2][i];
    l2 = l*l; l3 = l2*l;
    p2 = p*p; p3 = p2*p;
    h2 = h*h; h3 = h2*h;
    
    if (rpctype==0)       /* type RPC00A */
       {
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*p*l*h+
       rpcd[8]*l2+rpcd[9]*p2+rpcd[10]*h2+rpcd[11]*l3+
       rpcd[12]*l2*p+rpcd[13]*l2*h+rpcd[14]*l*p2+
       rpcd[15]*p3+rpcd[16]*p2*h+rpcd[17]*l*h2+
       rpcd[18]*p*h2+rpcd[19]*h3;
       clsq[i] = 1.0;
       clsq[n_data_pt+i] = l;
       clsq[n_data_pt*2+i] = p;
       clsq[n_data_pt*3+i] = h;
       clsq[n_data_pt*4+i] = l*p;
       clsq[n_data_pt*5+i] = l*h;
       clsq[n_data_pt*6+i] = p*h;
       clsq[n_data_pt*7+i] = p*l*h;
       clsq[n_data_pt*8+i] = l2;
       clsq[n_data_pt*9+i] = p2;
       clsq[n_data_pt*10+i] = h2;
       clsq[n_data_pt*11+i] = l3;
       clsq[n_data_pt*12+i] = l2*p;
       clsq[n_data_pt*13+i] = l2*h;
       clsq[n_data_pt*14+i] = l*p2;
       clsq[n_data_pt*15+i] = p3;
       clsq[n_data_pt*16+i] = p2*h;
       clsq[n_data_pt*17+i] = l*h2;
       clsq[n_data_pt*18+i] = p*h2;
       clsq[n_data_pt*19+i] = h3;
       }
    else                   /* type RPC00B */
       {
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*l2+
       rpcd[8]*p2+rpcd[9]*h2+rpcd[10]*p*l*h+rpcd[11]*l3+
       rpcd[12]*l*p2+rpcd[13]*l*h2+rpcd[14]*l2*p+
       rpcd[15]*p3+rpcd[16]*p*h2+rpcd[17]*l2*h+
       rpcd[18]*p2*h+rpcd[19]*h3;
       clsq[i] = 1.0;
       clsq[n_data_pt+i] = l;
       clsq[n_data_pt*2+i] = p;
       clsq[n_data_pt*3+i] = h;
       clsq[n_data_pt*4+i] = l*p;
       clsq[n_data_pt*5+i] = l*h;
       clsq[n_data_pt*6+i] = p*h;
       clsq[n_data_pt*7+i] = l2;
       clsq[n_data_pt*8+i] = p2;
       clsq[n_data_pt*9+i] = h2;
       clsq[n_data_pt*10+i] = p*l*h;
       clsq[n_data_pt*11+i] = l3;
       clsq[n_data_pt*12+i] = l*p2;
       clsq[n_data_pt*13+i] = l*h2;
       clsq[n_data_pt*14+i] = l2*p;
       clsq[n_data_pt*15+i] = p3;
       clsq[n_data_pt*16+i] = p*h2;
       clsq[n_data_pt*17+i] = l2*h;
       clsq[n_data_pt*18+i] = p2*h;
       clsq[n_data_pt*19+i] = h3;
       }

    clsqdep[i] = ypar[i]*denom;
    }
  
  return 1;
}

int rpc_f11()
{
  double rpcn[20],rpcd[20],l,p,h,l2,p2,h2,l3,p3,h3,numer,denom;
  int strcmp();
  size_t i;

  for (i=0;i<20;i++) rpcn[i] = tguess[i+rpc_linsmp*40];
  for (i=0;i<20;i++) rpcd[i] = tguess[i+20+rpc_linsmp*40];

  for (i=0;i<n_data_pt;i++)
    {
    l = xpar[0][i];
    p = xpar[1][i];
    h = xpar[2][i];
    l2 = l*l; l3 = l2*l;
    p2 = p*p; p3 = p2*p;
    h2 = h*h; h3 = h2*h;
    
    if (rpctype==0)       /* type RPC00A */
       {
       numer = rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*p*l*h+
       rpcn[10]*h2+
       rpcn[13]*l2*h+
       rpcn[16]*p2*h+rpcn[17]*l*h2+
       rpcn[18]*p*h2+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*p*l*h+
       rpcd[8]*l2+rpcd[9]*p2+rpcd[10]*h2+rpcd[11]*l3+
       rpcd[12]*l2*p+rpcd[13]*l2*h+rpcd[14]*l*p2+
       rpcd[15]*p3+rpcd[16]*p2*h+rpcd[17]*l*h2+
       rpcd[18]*p*h2+rpcd[19]*h3;
       clsq[i] = 1.0;
       clsq[n_data_pt+i] = l;
       clsq[n_data_pt*2+i] = p;
       clsq[n_data_pt*3+i] = l*p;
       clsq[n_data_pt*4+i] = l2;
       clsq[n_data_pt*5+i] = p2;
       clsq[n_data_pt*6+i] = l3;
       clsq[n_data_pt*7+i] = l2*p;
       clsq[n_data_pt*8+i] = l*p2;
       clsq[n_data_pt*9+i] = p3;
       clsq[n_data_pt*10+i] = h;
       }
    else                   /* type RPC00B */
       {
       numer = rpcn[5]*l*h+rpcn[6]*p*h+
       rpcn[9]*h2+rpcn[10]*p*l*h+
       rpcn[13]*l*h2+
       rpcn[16]*p*h2+rpcn[17]*l2*h+
       rpcn[18]*p2*h+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*l2+
       rpcd[8]*p2+rpcd[9]*h2+rpcd[10]*p*l*h+rpcd[11]*l3+
       rpcd[12]*l*p2+rpcd[13]*l*h2+rpcd[14]*l2*p+
       rpcd[15]*p3+rpcd[16]*p*h2+rpcd[17]*l2*h+
       rpcd[18]*p2*h+rpcd[19]*h3;
       clsq[i] = 1.0;
       clsq[n_data_pt+i] = l;
       clsq[n_data_pt*2+i] = p;
       clsq[n_data_pt*3+i] = l*p;
       clsq[n_data_pt*4+i] = l2;
       clsq[n_data_pt*5+i] = p2;
       clsq[n_data_pt*6+i] = l3;
       clsq[n_data_pt*7+i] = l*p2;
       clsq[n_data_pt*8+i] = l2*p;
       clsq[n_data_pt*9+i] = p3;
       clsq[n_data_pt*10+i] = h;
       }

    clsqdep[i] = ypar[i]*denom-numer;
    }
  
  return 1;
}

int rpc_fzt()
{
  double rpcn[20],rpcd[20],l,p,h,l2,p2,h2,l3,p3,h3,numer,denom;
  int strcmp();
  size_t i;

  for (i=0;i<20;i++) rpcn[i] = tguess[i+rpc_linsmp*40];
  for (i=0;i<20;i++) rpcd[i] = tguess[i+20+rpc_linsmp*40];

  for (i=0;i<n_data_pt;i++)
    {
    l = xpar[0][i];
    p = xpar[1][i];
    h = xpar[2][i];
    l2 = l*l; l3 = l2*l;
    p2 = p*p; p3 = p2*p;
    h2 = h*h; h3 = h2*h;
    
    if (rpctype==0)       /* type RPC00A */
       {
       numer = rpcn[1]*l+rpcn[2]*p+
       rpcn[4]*l*p+
       rpcn[8]*l2+rpcn[9]*p2+rpcn[11]*l3+
       rpcn[12]*l2*p+rpcn[14]*l*p2+
       rpcn[15]*p3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*p*l*h+
       rpcd[8]*l2+rpcd[9]*p2+rpcd[10]*h2+rpcd[11]*l3+
       rpcd[12]*l2*p+rpcd[13]*l2*h+rpcd[14]*l*p2+
       rpcd[15]*p3+rpcd[16]*p2*h+rpcd[17]*l*h2+
       rpcd[18]*p*h2+rpcd[19]*h3;
       clsq[i] = 1.0;
       clsq[n_data_pt+i] = h;
       clsq[n_data_pt*2+i] = l*h;
       clsq[n_data_pt*3+i] = p*h;
       clsq[n_data_pt*4+i] = p*l*h;
       clsq[n_data_pt*5+i] = h2;
       clsq[n_data_pt*6+i] = l2*h;
       clsq[n_data_pt*7+i] = p2*h;
       clsq[n_data_pt*8+i] = l*h2;
       clsq[n_data_pt*9+i] = p*h2;
       clsq[n_data_pt*10+i] = h3;
       }
    else                   /* type RPC00B */
       {
       numer = rpcn[1]*l+rpcn[2]*p+
       rpcn[4]*l*p+rpcn[7]*l2+
       rpcn[8]*p2+rpcn[11]*l3+
       rpcn[12]*l*p2+rpcn[14]*l2*p+
       rpcn[15]*p3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*l2+
       rpcd[8]*p2+rpcd[9]*h2+rpcd[10]*p*l*h+rpcd[11]*l3+
       rpcd[12]*l*p2+rpcd[13]*l*h2+rpcd[14]*l2*p+
       rpcd[15]*p3+rpcd[16]*p*h2+rpcd[17]*l2*h+
       rpcd[18]*p2*h+rpcd[19]*h3;
       clsq[i] = 1.0;
       clsq[n_data_pt+i] = h;
       clsq[n_data_pt*2+i] = l*h;
       clsq[n_data_pt*3+i] = p*h;
       clsq[n_data_pt*4+i] = h2;
       clsq[n_data_pt*5+i] = p*l*h;
       clsq[n_data_pt*6+i] = l*h2;
       clsq[n_data_pt*7+i] = p*h2;
       clsq[n_data_pt*8+i] = l2*h;
       clsq[n_data_pt*9+i] = p2*h;
       clsq[n_data_pt*10+i] = h3;
       }

    clsqdep[i] = ypar[i]*denom-numer;
    }
  
  return 1;
}

int rpc_fz2()
{
  double rpcn[20],rpcd[20],l,p,h,l2,p2,h2,l3,p3,h3,numer,denom;
  int strcmp();
  size_t i;

  for (i=0;i<20;i++) rpcn[i] = tguess[i+rpc_linsmp*40];
  for (i=0;i<20;i++) rpcd[i] = tguess[i+20+rpc_linsmp*40];

  for (i=0;i<n_data_pt;i++)
    {
    l = xpar[0][i];
    p = xpar[1][i];
    h = xpar[2][i];
    l2 = l*l; l3 = l2*l;
    p2 = p*p; p3 = p2*p;
    h2 = h*h; h3 = h2*h;
    
    if (rpctype==0)       /* type RPC00A */
       {
       numer = rpcn[1]*l+rpcn[2]*p+
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
       clsq[i] = 1.0;
       clsq[n_data_pt+i] = h;
       }
    else                   /* type RPC00B */
       {
       numer = rpcn[1]*l+rpcn[2]*p+
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
       clsq[i] = 1.0;
       clsq[n_data_pt+i] = h;
       }

    clsqdep[i] = ypar[i]*denom-numer;
    }
  
  return 1;
}

int rpc_fz1()
{
  double rpcn[20],rpcd[20],l,p,h,l2,p2,h2,l3,p3,h3,numer,denom;
  int strcmp();
  size_t i;

  for (i=0;i<20;i++) rpcn[i] = tguess[i+rpc_linsmp*40];
  for (i=0;i<20;i++) rpcd[i] = tguess[i+20+rpc_linsmp*40];

  for (i=0;i<n_data_pt;i++)
    {
    l = xpar[0][i];
    p = xpar[1][i];
    h = xpar[2][i];
    l2 = l*l; l3 = l2*l;
    p2 = p*p; p3 = p2*p;
    h2 = h*h; h3 = h2*h;
    
    if (rpctype==0)       /* type RPC00A */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+
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
       clsq[i] = h;
       }
    else                   /* type RPC00B */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+
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
       clsq[i] = h;
       }

    clsqdep[i] = ypar[i]*denom-numer;
    }
  
  return 1;
}

/* end of user routines ************************************************/

void main44(void)
{
   int i,j,datacol[20],coeffcol[100],unit,indcount,coeffcount,coldef;
   int ibis,status,clen,depcol,rescol,concol,numlsq,ier;
   int unit2,ibis2,clen2,solcount,soldef;
   int dummy,dct,noprint,uptr,lptr,igroup,strcmp();
   int (*fptr)() = NULL;
   float *concolv,groupnbr;
   double **sout,eps,*solnck;
   char rpctypstr[2],rpclsstr[2];
   
   zifmessage("ibislsqf version Fri Dec 28 2007");
   
   /* get the basic parameters */
   
   zvparm("func",func_name,&dct,&dummy,1,0);
   zvparm("rpctype",rpctypstr,&dct,&dummy,1,0);
   if (rpctypstr[0]=='B') rpctype = 1; else rpctype = 0;
   zvparm("rpcls",rpclsstr,&dct,&dummy,1,0);
   if (rpclsstr[0]=='S') rpc_linsmp = 1; else rpc_linsmp = 0;
   if (rpctype==0) printf("RPCTYPE=A\n");
      else printf("RPCTYPE=B\n");
   if (rpc_linsmp==0) printf("SOLVING FOR LINE\n");
      else printf("SOLVING FOR SAMP\n");
   printf("rpctype,rpc_linsmp %d %d\n",rpctype,rpc_linsmp);
   status = zvp("depcol",&depcol,&dummy);
   status = zvp("rescol",&rescol,&dummy);
   status = zvp("concol",&concol,&dummy);
   if (concol!=0)
     zmabend("multiple solutions prohibited for now");
   noprint = zvptst("noprint");
   
   zvparm("datacol",datacol,&indcount,&coldef,20,0);
   zvparm("coeffcol",coeffcol,&coeffcount,&coldef,100,0);
   zvparm("solcol",solcol,&solcount,&soldef,5,0);
    
   /* read in data from the ibis interface file */

   status = zvunit(&unit,"inp",1, NULL);
   status = IBISFileOpen(unit,&ibis,"update",0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nr",&clen,1,1,0);
   n_data_pt = clen;

   mz_alloc2((unsigned char ***)&xpar,indcount,clen,8);
   mz_alloc1((unsigned char **)&clsqdep,clen,8);
   mz_alloc1((unsigned char **)&ypar,clen,8);
   mz_alloc1((unsigned char **)&concolv,clen,8);
   
   if (rescol!=0) mz_alloc1((unsigned char **)&rout,clen,8);
   
   for (i=0;i<indcount;i++)
      {
      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",datacol[i]);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnRead(ibis,(char *)xpar[i],datacol[i],1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
      }
   status = IBISColumnSet(ibis,"U_FORMAT","DOUB",depcol);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char *)ypar,depcol,1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   if (concol>0)
      {
      status = IBISColumnSet(ibis,"U_FORMAT","REAL",concol);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnRead(ibis,(char *)concolv,concol,1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
      }
   else for (i=0;i<clen;i++) concolv[i] = 1.0;
   
   /* read in solution tguess from the second ibis interface file */

   status = zvunit(&unit2,"inp",2, NULL);
   status = IBISFileOpen(unit2,&ibis2,"update",0,0,0,0);
   if (status!=1) IBISSignalU(unit2,status,1);
   IBISFileGet(ibis2,"nr",&clen2,1,1,0);
   n_guess = clen2;
   n_ind_var = clen2;   /* may be reset later */
   mz_alloc1((unsigned char **)&tguess,clen2,8);
   mz_alloc1((unsigned char **)&solnck,clen2,8);
   printf("\n");
   status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",solcol[0]);
   if (status!=1) IBISSignal(ibis2,status,1);
   status = IBISColumnRead(ibis2,(char *)tguess,solcol[0],1,clen2);
   if (status!=1) IBISSignal(ibis2,status,1);
   if (solcol[2]>0)
      {
      status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",solcol[2]);
      if (status!=1) IBISSignal(ibis2,status,1);
      status = IBISColumnRead(ibis2,(char *)solnck,solcol[2],1,clen2);
      if (status!=1) IBISSignal(ibis2,status,1);
      }
   
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
      eps = 1.e-7;
     
      /* set up function pointers and verify data columns */
      
      if (strcmp(func_name,"RPCN20")==0)
         {
         n_ind_var = 20;
         if (n_guess!=80) zmabend("wrong length for guess vector");
         if (indcount!=3) zmabend("wrong number of input data columns");
         for (i=0;i<20;i++)solnck[i] = solnck[i+rpc_linsmp*40];
         fptr = &rpc_f20;
         }
      if (strcmp(func_name,"RPCN10")==0)
         {
         n_ind_var = 10;
         if (n_guess!=80) zmabend("wrong length for guess vector");
         if (indcount!=3) zmabend("wrong number of input data columns");
         rpctran(solnck,solnck,rpc_linsmp*40);
         fptr = &rpc_f10;
         }
      if (strcmp(func_name,"RPCN11")==0)
         {
         n_ind_var = 11;
         if (n_guess!=80) zmabend("wrong length for guess vector");
         if (indcount!=3) zmabend("wrong number of input data columns");
         rpc11tran(solnck,solnck,rpc_linsmp*40);
         fptr = &rpc_f11;
         }
      if (strcmp(func_name,"RPCNZT")==0)
         {
         n_ind_var = 11;
         if (n_guess!=80) zmabend("wrong length for guess vector");
         if (indcount!=3) zmabend("wrong number of input data columns");
         rpcztran(solnck,solnck,rpc_linsmp*40);
         fptr = &rpc_fzt;
         }
      if (strcmp(func_name,"RPCNZ2")==0)
         {
         n_ind_var = 2;
         if (n_guess!=80) zmabend("wrong length for guess vector");
         if (indcount!=3) zmabend("wrong number of input data columns");
         rpc2tran(solnck,solnck,rpc_linsmp*40);
         fptr = &rpc_fz2;
         }
      if (strcmp(func_name,"RPCNZ1")==0)
         {
         n_ind_var = 1;
         if (n_guess!=80) zmabend("wrong length for guess vector");
         if (indcount!=3) zmabend("wrong number of input data columns");
         rpc1tran(solnck,solnck,rpc_linsmp*40);
         fptr = &rpc_fz1;
         }
      /*if (strcmp(func_name,"RPCROLL")==0)
         {
         n_ind_var = 2;
         if (n_guess!=80) zmabend("wrong length for guess vector");
         if (indcount!=3) zmabend("wrong number of input data columns");
         rpcrtran(solnck,solnck,rpc_linsmp*40);
         fptr = &rpc_froll;
         }??alz*/
      
      /* the linear least squares solver */
      
      mz_alloc1((unsigned char **)&clsq,n_ind_var*clen,8);
      ier = 0;
      fptr();
      lsqfit(clsq,clsqdep,n_data_pt,n_ind_var,csol,eps,&ier);
      
      /* transfer the solution back to tguess vector */
      
      if (strcmp(func_name,"RPCN20")==0) for (i=0;i<20;i++)
         tguess[i+rpc_linsmp*40] = csol[i];
      if (strcmp(func_name,"RPCN10")==0)
         rpctranb(tguess,csol,rpc_linsmp*40);
      if (strcmp(func_name,"RPCN11")==0)
         rpc11tranb(tguess,csol,rpc_linsmp*40);
      if (strcmp(func_name,"RPCNZT")==0)
         rpcztranb(tguess,csol,rpc_linsmp*40);
      if (strcmp(func_name,"RPCNZ2")==0)
         rpc2tranb(tguess,csol,rpc_linsmp*40);
      if (strcmp(func_name,"RPCNZ1")==0)
         rpc1tranb(tguess,csol,rpc_linsmp*40);
      /*if (strcmp(func_name,"RPCROLL")==0)
         rpcrolltranb(tguess,csol,rpc_linsmp*40);??alz*/
      
      /* printing if requested */
      
      if (!noprint)
         {
         printf("\nseq  concol    solution coefficient\n\n");
         for (i=0;i<n_ind_var;i++)
            if (ier==0) printf("%3d %7.2f %24.15e\n",
                i+1,groupnbr,csol[i]);
            else printf("%3d %7.2f %24.15e\n",
                i+1,groupnbr,-999.0);
         printf("\n");
         }
      
      /* calculate the output data, rout now calc in non-lin functions */
      
      if (coeffcol[0]!=0)
         {
         if (coeffcount!=n_ind_var&&coldef==0)
            zmabend("Count for parameter COEFFCOL wrong");
         mz_alloc2((unsigned char ***)&sout,n_ind_var,clen,8);
         for (i=0;i<n_ind_var;i++)
            for (j=lptr;j<uptr;j++)
               if (ier==0) sout[i][j] = csol[i];
               else sout[i][j] = -999.0;
         }
      }
     
   /* Output desired columns to the ibis interface files */
   
   if (coeffcol[0]!=0)
      {
      for (i=0;i<n_ind_var;i++)
         {
         status = IBISColumnSet(ibis,"U_FORMAT","DOUB",coeffcol[i]);
         if (status!=1) IBISSignal(ibis,status,1);
         status = IBISColumnWrite(ibis,(char*)(sout[i]),coeffcol[i],1,clen);
         if (status!=1) IBISSignal(ibis,status,1);
         }
      }
   if (rescol!=0)
      {
      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",rescol);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnWrite(ibis,(char*)rout,rescol,1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
      }

   status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",solcol[1]);
   if (status!=1) IBISSignal(ibis2,status,1);
   status = IBISColumnWrite(ibis2,(char*) tguess,solcol[1],1,clen2);
   if (status!=1) IBISSignal(ibis2,status,1);
         
   status = IBISFileClose(ibis,0);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISFileClose(ibis2,0);
   if (status!=1) IBISSignal(ibis2,status,1);
}
