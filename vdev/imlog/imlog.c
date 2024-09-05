#include <math.h>
#include <stdio.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

#include "carto/cartoVicarProtos.h"
#include "carto/cartoMemUtils.h"

#ifdef __i386__
/* ntohl and ntohs byte order converters */
#include <netinet/in.h>
#endif

/*  log raw image to VICAR   A. Zobrist    10/20/05   */

void main44(void)
{
   int i,j,nl,ns,lead_rm,parmct,parmdf,status,o_unit,rns;
   int linehdr,linetail,lineskp,swab;
   char infilename[100],fmt_str[10];
   unsigned char *buf,tmp;
   FILE *mifcb1;
   
   zifmessage("imlog version Wed Jan  2 2008");
   
   /* open tae, fetch params, and open files */

   zvparm("inp",infilename,&parmct,&parmdf,1,100);
   zvparm("format",fmt_str,&parmct,&parmdf,4,0);
   zvp("nl",&nl,&parmct);
   zvp("ns",&ns,&parmct);
   zvp("lead_rm",&lead_rm,&parmct);
   zvp("linehdr",&linehdr,&parmct);
   zvp("linetail",&linetail,&parmct);
   lineskp = linehdr+linetail;
   if (strcmp(fmt_str,"BYTE")==0)
      rns = ns+lineskp;
   else if (strcmp(fmt_str,"HALF")==0)
      rns = 2*ns+lineskp;
   else
      rns = 4*ns+lineskp;
   swab = zvptst("swab");
   
   /* open the raw file */
   
   mifcb1 = fopen(infilename,"r");
   if (mifcb1==0) zmabend("input file not found");
   
   fseek(mifcb1,(long)(lead_rm+linehdr),0);

   /* open the output and allocate buffer*/
   
   status=zvunit(&o_unit,"OUT",1, NULL);
   status=zvopen(o_unit,"U_NL",nl,"U_NS",ns,
        "U_FORMAT",fmt_str,"O_FORMAT",fmt_str,
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA", NULL);
   
   mz_alloc1((unsigned char **)&buf,rns,1);
   
   /* copy operation */
   
   for (i=0;i<nl;i++)
      {
      fread(buf,1,rns,mifcb1);

      /* byte swap here */
      if (swab)
         {
         if (strcmp(fmt_str,"HALF")==0)
            for (j=0;j<ns;j+=2)
               {tmp = buf[j]; buf[j] = buf[j+1]; buf[j+1] = tmp; }
         else if (strcmp(fmt_str,"FULL")==0)
            for (j=0;j<ns;j+=4)
               {tmp = buf[j]; buf[j] = buf[j+1]; buf[j+1] = buf[j+2];
               buf[j+2] = buf[j+3]; buf[j+3] = buf[j+4]; buf[j+4] = tmp; }
         }

      zvwrit(o_unit,buf,"LINE",i+1,"SAMP",1,"NSAMPS",ns, NULL);
      }
   
   fclose(mifcb1);
   zvclose(o_unit, NULL);
   return;
}
