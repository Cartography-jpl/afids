#include <stdio.h>

#include "vicmain_c.h"
#include "applic.h"
#include "taeconf.inp"
#include "parblk.inc"
#include "defines.h"

#include "carto/cartoVicarProtos.h"
#include "carto/cartoMemUtils.h"
#include "carto/cartoTaeUtils.h"
#include "carto/time_conversion.h"
#include "carto/time_utils.h"
#include "carto/burl.h" /* for ERR and OK */

/*  getdelta program   Walt Bunch, A. Zobrist    05/29/08   */

static int beyond( char * leftTime, char * rightTime )
{
  int MMl, ddl, yyyyl, hhl, mml, ssl;
  int MMr, ddr, yyyyr, hhr, mmr, ssr;

  if ( sscanf( leftTime, "%02d-%02d-%04d %02d:%02d:%02d", &MMl, &ddl, &yyyyl, &hhl, &mml, &ssl ) != 6 ) {
    printf( "failed to scan %s\n", leftTime );
    return 0;
  }

  if ( sscanf( rightTime, "%02d-%02d-%04d %02d:%02d:%02d", &MMr, &ddr, &yyyyr, &hhr, &mmr, &ssr ) != 6 ) {
    printf( "failed to scan %s\n", rightTime );
    return 0;
  }

  /*
    printf( "beyond parsed\n%sto\n%02d-%02d-%04d %02d:%02d:%02d\n", leftTime, MMl, ddl, yyyyl, hhl, mml, ssl );
    printf( "beyond parsed\n%s\nto\n%02d-%02d-%04d %02d:%02d:%02d\n", rightTime, MMr, ddr, yyyyr, hhr, mmr, ssr );
  */

  if ( yyyyl > yyyyr )
    return 1;
  if ( yyyyl < yyyyr )
    return 0;

  /* must be same year */

  if ( MMl > MMr )
    return 1;
  if ( MMl < MMr )
    return 0;

  /* must be same month */

  if ( ddl > ddr )
    return 1;
  if ( ddl < ddr )
    return 0;

  /* must be same day */

  if ( hhl > hhr )
    return 1;
  if ( hhl < hhr )
    return 0;

  /* must be same hour */

  if ( mml > mmr )
    return 1;
  if ( mml < mmr )
    return 0;

  /* must be same minute */

  if ( ssl > ssr )
    return 1;

  return 0;
}


/*
 * path is path to text data file holding timestamps and deltas
 * timeStamp is formatted as "MM-DD-YYYY HH:MM:SS"
 * File lines are expected to be formatted like the following:
 * MM-DD-YYYY HH:MM:SS -0.1234567
 * where the minus sign in the delta may be omitted.
 */
double findDelta( char* path, char* timeStamp )
{
#define BUF_SIZE 100
  char buf[ BUF_SIZE ];
  FILE* f;
  double val, retval = -999.0;

  if ( ( f = fopen( path, "r" ) ) ) {
    while ( fgets( buf, BUF_SIZE, f) ) {
      if ( retval > -900.0 && beyond( buf, timeStamp ) ) { /* take the first time if timeStamp preceeds it */
	printf( "%s is beyond %s\n", buf, timeStamp );
	break;
      } else if ( sscanf( buf + 20, "%lf", &val ) == 1 ) /* skip timestamp and parse double delta */
	retval = val;		/* found a new time that is not greater than timeStamp */
	printf( "retval set to %.8lf\n", retval );
    }
  }

  if ( f )
    fclose( f );

  return retval;
}

   /* this was the original Walt call
   printf( "Searching %s for %s ...\n", argv[ 1 ], argv[ 2 ] );
   printf( "Found %.8lf\n", findDelta( argv[ 1 ], argv[ 2 ] ) );*/

void main44(void)
{
   int ct,def;
   double itim,fud;
   char path[100],*tstamp=NULL,*tstamp2=NULL;
   char leapFile[100];
   int leapFileCount;
   int parmdf;
   
   zifmessage("getdelta version Tue Jul 22 2008");
   
   /* get parms */
   
   zvparm("path",path,&ct,&def,1,0);
   zvparmd("itim",&itim,&ct,&def,1,0);
   zvparm("leapfile", leapFile, &leapFileCount, &parmdf, 1, 99);
      
   if ( initialize_leap_second_table( leapFile ) == ERR )
     zmabend( "initialize_leap_second_table failed" );
   acs_to_utc_iso_time_string(itim,&tstamp);
   utc_iso_time_string_to_dut1tbl_time_string(tstamp,&tstamp2);
   
   /* fraction of second truncation OK for purposes of delta ut1 (slowly var) */

   fud = findDelta(path,tstamp2);
   
   mq_out_real("val",fud);
   
   return;
}
