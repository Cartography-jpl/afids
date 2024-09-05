#include <math.h>
#include <stdio.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

#include "carto/cartoVicarProtos.h"

#ifdef __i386__
/* ntohl and ntohs byte order converters */
#include <netinet/in.h>
#endif

/*  log DMA DTED to VICAR   A. Zobrist    09/24/02   */

void main44(void)
{
   int i,nl,ns,countz,lead_rm=0,level=0;
   int parmct,parmdf,lon,lat,status,o_unit,i_unit=0,hshift;
   char infilename[100],hemi1[2],hemi2[2];
   unsigned char hdbuf[10000];
   short int buf[3601];
   FILE *mifcb1;
   
   zifmessage("dtedvlog version Thu Jan  3 2008");
   
   /* open tae, fetch params, and open files */

   zvparm("inp",infilename,&parmct,&parmdf,1,100);
   zvparm("LON",&lon,&parmct,&parmdf,1,0);
   zvparm("LAT",&lat,&parmct,&parmdf,1,0);
   zvparm("HEMI1",hemi1,&parmct,&parmdf,1,2);
   zvparm("HEMI2",hemi2,&parmct,&parmdf,1,2);
   if (hemi2[0]=='s') hshift = 1; else hshift = 0;
   if (lat<0) zmabend("negative lat; use hemi2=s and positive lat");
   
   if (infilename[strlen(infilename)-1]=='1') level = 1;
   if (infilename[strlen(infilename)-1]=='2') level = 2;
   printf("level %d detected\n",level);

   ns=1201;
   if (lat>=(80+hshift)) nl=201;
   else if (lat>=(75+hshift)) nl=301;
   else if (lat>=(70+hshift)) nl=401;
   else if (lat>=(50+hshift)) nl=601;
      else nl=1201;
   if (level==2) { nl = 3*nl-2; ns = 3*ns-2; }
   
   /* optical disc seem to have varying length ascii headers with no
      zeros leading up to the first line which begins with several bytes
      containing zero (actually the line number in float format) */
   
   mifcb1 = fopen(infilename,"r");
   if (mifcb1==0) 
     zmabend("input file not found, possible missing leading zero in lon or lat");
   fread(hdbuf,1,10000,mifcb1);
   countz = 0;
   for (i=0;i<10000;i++)
      {
      if (hdbuf[i]==0)
	 {
	 countz++;
	 if (countz>=4&&i%4==3) { lead_rm = i+1; break; }
	 }
      else countz = 0;
      }
   printf("data starts at byte %d\n",lead_rm);
   rewind(mifcb1);
   fclose(mifcb1);

   /* reopen input */

   mifcb1 = fopen(infilename,"r");
   fseek(mifcb1,(long)lead_rm,1);

   /* open the output */
   
   status=zvunit( &o_unit,"OUT",1, NULL);
   status=zvopen( o_unit,"U_NL",nl,"U_NS",ns,
        "U_FORMAT","HALF","O_FORMAT","HALF",
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA", NULL);
   
   /* copy operation */
   
   for (i=0;i<nl;i++)
      {
      fread(buf,2,ns,mifcb1);

#ifdef __i386__
    /* 16-bit short swapping */
    /* make it little endian for i80x86 */
 {
   int j;
   for (j=0;j<ns;j++) buf[j] = ntohs(buf[j]);
 }
#endif

      zvwrit(o_unit,buf,"LINE",i+1,"SAMP",1,"NSAMPS",ns, NULL);
      if (i<(nl-1)) fseek(mifcb1,(long)12,1);
      }
   
   zvclose(i_unit, NULL);
   zvclose(o_unit, NULL);
   return;
}
