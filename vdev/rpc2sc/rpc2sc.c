#include <math.h>
#include <string.h>
#include <ctype.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"

#include "carto/cartoVicarProtos.h"
#include "carto/cartoStrUtils.h"
#include "carto/cartoMemUtils.h"
#include "carto/cartoGtUtils.h"
#include "carto/cartoRpcUtils.h"

#include "carto/rpc_to_wpp.h"
#include "carto/estimate_wpp_camera.h"

#define ERR -1

/*  compute spacecraft ephemeris from rpc   A. Zobrist/M Burl    11/01/06   */

void main44(void)
{
   int i,j,k,cols[2],colcount,count,dummy,r_plh,rpctype=0;
   int status,labnl,labns,unit,ounit,ibis,oibis;
   char *labelstr,*p;
   double u0,v0,altitude,plh[6],pout[8],qout[8],rpc_vec[95],txfr[2];
   double m,q,tau,theta,phi,psi,bu,bv,alt,proj_mx[16],bufout[2];
   
   zifmessage("rpc2sc version Thu Jan  3 2008");
   
   /* get some parms */
   
   zvparm("cols",cols,&colcount,&dummy,2,0);
   zvparmd("lonrange",txfr,&count,&dummy,2,0);
   plh[1] = txfr[0];
   plh[4] = txfr[1];
   zvparmd("latrange",txfr,&count,&dummy,2,0);
   plh[0] = txfr[0];
   plh[3] = txfr[1];
   zvparmd("hrange",txfr,&count,&dummy,2,0);
   plh[2] = txfr[0];
   plh[5] = txfr[1];
   zvparmd("u0",&u0,&count,&dummy,1,0);
   zvparmd("v0",&v0,&count,&dummy,1,0);
   zvparmd("altitude",&altitude,&count,&dummy,1,0);
   
   r_plh = 2;
   alt = altitude;
   
   /* read the rpc's from the first input, one of two possible formats */
   
   if (cols[0]==0)
      {
      status = gtgetlab("inp",1,&labelstr,&labnl,&labns);
      for (j=0;j<13;j++) rpcrd(0,j,labelstr,&rpc_vec[0]);
      for (j=0;j<20;j++) rpcrd(14,j,labelstr,&rpc_vec[13]);
      for (j=0;j<20;j++) rpcrd(15,j,labelstr,&rpc_vec[33]);
      for (j=0;j<20;j++) rpcrd(16,j,labelstr,&rpc_vec[53]);
      for (j=0;j<20;j++) rpcrd(17,j,labelstr,&rpc_vec[73]);
      p = ms_find(labelstr,"NITF_CETAG=");
      if (p!=0)
         {
         if (strncmp(p,"RPC00A",6)==0)
            {
            rpctype = 1;
            printf("processing RPC00A\n");
            }
         if (strncmp(p,"RPC00B",6)==0)
            {
            rpctype = 2  ;
            printf("processing RPC00B\n");
            }
         }
      else
         {
         rpctype = 1;
         printf("processing RPC00A by default\n");
         }
      }
   else
      {
      status = zvunit(&unit,"inp",1, NULL);
      status = IBISFileOpen(unit,&ibis,"read",0,0,0,0);
      if (status!=1) IBISSignalU(unit,status,1);
      
      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[1]);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnRead(ibis,(char*)rpc_vec,cols[1],3,15);
      if (status!=1) IBISSignal(ibis,status,1);
      rpctype = (int)(rpc_vec[0]+0.01);
      
      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[0]);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnRead(ibis,(char*)(&rpc_vec[13]),cols[0],1,80);
      if (status!=1) IBISSignal(ibis,status,1);
      
      status = IBISFileClose(ibis,0);
      if (status!=1) IBISSignal(ibis,status,1);
      }
   
   for (i=0;i<19;i++)
      {
      printf("\n");
      for(j=0;j<5;j++) if (i*5+j<93) printf("%14.10f ",rpc_vec[i*5+j]);
      }
   printf("\n");  
   
   /* call the function */
   
   status = rpc_to_wpp(r_plh, plh, rpctype, rpc_vec, &m, &q, &tau, &theta, &phi, &psi, &bu, &bv);
   
   if (status == ERR) zmabend("rpc_to_wpp failed");
   
   printf("\n");
   printf("m =     %.15f\n", m);
   printf("q =     %.15f\n", q);
   printf("tau =   %.15f\n", tau);
   printf("theta = %.15f\n", theta);
   printf("phi =   %.15f\n", phi);
   printf("psi =   %.15f\n", psi);
   printf("bu =    %.15f\n", bu);
   printf("bv =    %.15f\n", bv);
   printf("\n");
   pout[0] = m;
   pout[1] = q;
   pout[2] = tau;
   pout[3] = theta;
   pout[4] = phi;
   pout[5] = psi;
   pout[6] = bu;
   pout[7] = bv;
   
   /* Calculate the projection matrix (for fun) and print */
   wpp_projection_matrix(theta, phi, psi, m, q, tau, bu, bv, proj_mx);
   k = 0;
   for (i = 0; i < 4; i++) {
     for (j = 0; j < 4; j++) {
       printf("%.15f ", proj_mx[k]);
       k++;
     }
     printf("\n");
   }
   printf("\n");

   /* Calculate rotation matrix for camera orientation and from that quaternion */
   wpp_attitude_quaternion(theta, phi, psi, &qout[0]);

   for (i = 0; i < 4; i++) printf("attitude quat[%d] = %.15f\n", i, qout[i]);
   printf("\n");

   /* Calculate position quaternion - scalar part will be 0 */
   wpp_position_quaternion(alt, u0, v0, m, q, tau, theta, phi, psi, bu, bv, &qout[4]);
   for (i = 0; i < 4; i++) printf("position quat[%d] = %.15f\n", i+4, qout[i+4]);
   printf("\n");
   
   /* Output points to the ibis interface file */
  
   status = zvunit(&ounit,"out",1, NULL);
   status = IBISFileUnit(ounit,&oibis,"write",8,2,0,"column");
   status = IBISFileSet(oibis,"fmt_default","doub",0);
   status = IBISFileUnitOpen(oibis);
   /*status = IBISFileOpen(ounit,&oibis,"write",8,2,0,0);*/
   if (status!=1) IBISSignalU(oibis,status,1);
   
   for (i=0;i<8;i++)
      {
      bufout[0] = qout[i];
      bufout[1] = pout[i];
      status = IBISColumnWrite(oibis,(char*)bufout,i+1,1,2);
      if (status!=1) IBISSignal(oibis,status,1);
      }
   
   status = IBISFileClose(oibis,0);
   if (status!=1) IBISSignal(oibis,status,1);
   
   return;
}
