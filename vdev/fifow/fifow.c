#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>

#include "vicmain_c.h"

#include "carto/cartoVicarProtos.h"

/*
  program fifow

  opens named pipe (parm "fifo") and writes string (parm "value") to
  the pipe; then closes pipe.
*/

#define _fifow_version_ "Sat Dec 29 2007"

void main44(void)
{
  int parmct, parmdf;
  char logIdBuf [50];
  char pipeName [99];
  char value [99];

  int err = 0;

  sprintf (logIdBuf, "fifow version %s", _fifow_version_);
  zifmessage (logIdBuf);

  /* fetch parms */
  zvparm ("pipename", pipeName, &parmct, &parmdf, 1, 99);
  zvparm ("value", value, &parmct, &parmdf, 1, 99);

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
    FILE * file = fopen( pipeName, "w" );
    err = errno;

    if ( ! file ) {
      fprintf( stderr, "Couldn't open fofo '%s' (error: %d)\n", pipeName, err );
      exit( err );
    }

    fwrite( value, sizeof( char ), strlen( value ), file );

    fclose( file );
  }

  return;
}
