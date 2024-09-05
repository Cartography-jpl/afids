#include <math.h>
#include <stdio.h>
#include <string.h>

#include "vicmain_c.h"

#include "carto/cartoVicarProtos.h"

void main44(void)
{
  int i_unit,parmct,parmdf,nelement,maxlen;
  int status,nlabfix;
  char property[100],key[33],valformat[9];
  char value[5000];
  char outpath[100];
  FILE * outfile = 0;
  
  zifmessage( "lab2asc version Mon Aug 18 2008" );
   
  /* get parameters, open file */
   
  zvparm("outpath",outpath,&parmct,&parmdf,1,99);
  zvparm("property",property,&parmct,&parmdf,1,99);
  
  status = zvunit(&i_unit,"INP",1, NULL);
  status = zvopen(i_unit,"OP","UPDATE",
	"OPEN_ACT","SA","IO_ACT","SA","TYPE","IMAGE", NULL);
  
  outfile = fopen( outpath, "w" );

  nlabfix = 0;
  while (1) {
     status=zlninfo(i_unit,key,valformat,&maxlen,
        &nelement,"ERR_ACT"," ", NULL);
     if (status!=1) break;
     if (strcmp(key,"PROPERTY")==0) continue;
     status=zlinfo(i_unit,"PROPERTY",key,valformat,
        &maxlen,&nelement,"PROPERTY",property,
        "ERR_ACT"," ", NULL);
     if (status!=1) continue;
     if (strcmp(key,"PROPERTY")==0) continue;
     /*if (strcmp(valformat,"STRING")!=0) continue;*/
     
     status=zlget(i_unit,"PROPERTY",key,value,"ERR_ACT","SA",
        "FORMAT","STRING","PROPERTY",property, NULL);

     fprintf( outfile, "%s=%s\n", key, value );

  };

  return;
}
