#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "vicmain_c.h"

#include "carto/cartoVicarProtos.h"

#define _vexlab_version_ "Thu Jan  3 2008"

void main44(void)
{
  int parmct, parmdf;
  char infilename [99];
  char metaName [99];
  FILE * metaData;
#define bufSize 100
  char buf[bufSize + 1];
  int vunit;
  char * p;
  char msgBuf[100];

  sprintf (msgBuf, "vexlab version %s", _vexlab_version_);
  zifmessage (msgBuf);
   
  /* fetch params */
  zvparm ("inp", infilename, &parmct, &parmdf, 1, 99);
  zvparm ("meta", metaName, &parmct, &parmdf, 1, 99);

  metaData = fopen (metaName, "r");

  /* open VICAR image */
  if (zvunit (&vunit, "INP", 1, NULL) != 1)
    zmabend ("zvunit failed");
  if (zvopen (vunit, "OP", "UPDATE", "OPEN_ACT", "SA", "IO_ACT", "SA", "O_FORMAT", "HALF", NULL) != 1)
    zmabend ("zvopen failed");

  while (! feof(metaData)) {
    fgets(buf, bufSize, metaData);

    if (*buf) {
      buf[strlen(buf) - 1] = 0; /* kill \n */

      p = strstr(buf, "=");
      if (p) {
	*p = 0;

	if (strcmp(buf, "NITF_ROWS") && strcmp(buf, "NITF_COLUMNS")) {
	  char * key = buf;
	  void * value = p+1;
	  double doubleVal = 0.0;

	  if (strstr( key, "NITF_Corner" )) {
	    sscanf( value, "%lf", & doubleVal );

	    value = & doubleVal;
	  }

	  if (zladd (vunit, "PROPERTY",
		     key, value,
		     "PROPERTY", "GEOTIFF",
		     "FORMAT", value==&doubleVal?"DOUB":"STRING", NULL) != 1) {
	    sprintf (msgBuf, "addGTKey failed to add a label for key %s, value %s", key, (char*) value);
	    zmabend (msgBuf);
	  }
	}
      }
    }
  }

  /* done with meta data */
  fclose(metaData);

  /* done with VICAR image */
  zvclose (vunit, NULL);
}
