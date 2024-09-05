#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>

#include "vicmain_c.h"
#include "taeconf.inp"
#include "parblk.inc"

#include "carto/cartoTaeUtils.h"
#include "carto/cartoVicarProtos.h"

/*
  program fifor

  opens named pipe (parm "fifo") and reads string into parm "strvar";
  then closes pipe.
*/

#define _fifor_version_ "Sat Dec 29 2007"

void main44(void)
{
  int parmct, parmdf;
  char logIdBuf [50];
  char pipeName [99];
  char value [100];

  int err = 0;

  sprintf (logIdBuf, "fifor version %s", _fifor_version_);
  zifmessage (logIdBuf);

  /* fetch parms */
  zvparm ("pipename", pipeName, &parmct, &parmdf, 1, 99);

  {
    struct stat buf;

    if ( stat( pipeName, &buf ) == -1 ) {
      err = errno;
      fprintf( stderr, "FIFO does not exist: '%s' (error: %d )\n", pipeName, err );
      exit( err );
    } else if ( S_ISFIFO( buf.st_mode ) == 0 ) {
      err = errno;
      fprintf( stderr, "File: '%s' is not a FIFO. (error: %d)\n", pipeName, err );
      exit( err );
    }
  }

  {
    FILE * file = fopen( pipeName, "r" );
    int bytesRead;
    err = errno;

    if ( ! file ) {
      fprintf( stderr, "Couldn't open fofo '%s' (error: %d)\n", pipeName, err );
      exit( err );
    }

    bytesRead = fread( value, sizeof( char ), 99, file );

    if ( bytesRead > 0 ) {
      value[ bytesRead ] = '\0';
      mq_out_string ("strvar", value, 99);
    }

    fclose( file );
  }

  return;
}
