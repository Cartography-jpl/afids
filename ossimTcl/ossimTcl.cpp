// Indicate we are using large files (64 bit) on linux.
#define _LARGEFILE_SOURCE
#define _LARGEFILE64_SOURCE
#define _FILE_OFFSET_BITS 64

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <sstream>
#include <string>

#include <ossim/base/ossimFilename.h>
#include <ossim/imaging/ossimImageHandlerFactory.h>
#include <ossim/imaging/ossimImageHandlerRegistry.h>
#include <ossim/imaging/ossimCibCadrgTileSource.h>
#include <ossim/imaging/ossimImageHandler.h>
#include <ossim/imaging/ossimBandSelector.h>
#include <ossim/imaging/ossimImageSourceSequencer.h>
#include <ossim/imaging/ossimImageGeometry.h>
#include <ossim/support_data/ossimNitfFile.h>
#include <ossim/support_data/ossimNitfTagInformation.h>
#include <ossim/support_data/ossimNitfImageHeader.h>
#include <ossim/support_data/ossimNitfTagFactoryRegistry.h>
#include <ossim/imaging/ossimNitfWriter.h>
#include <ossim/support_data/ossimNitfRpcATag.h>
#include <ossim/support_data/ossimNitfRpcBTag.h>
#include <ossim/support_data/ossimNitfUse00aTag.h>
#include <ossim/support_data/ossimNitfUse26aTag.h>
#ifdef INCLUDE_AFIDS_A
#include <ossim/support_data/afids_a_1.h>
#endif
#include <ossim/base/ossimStringProperty.h>
#include <ossim/base/ossimContainerProperty.h>
#include <ossim/base/ossimTrace.h>
#include <ossim/base/ossimBooleanProperty.h>
#include <ossim/base/ossimKeywordNames.h>
#include <ossim/base/ossimRectilinearDataObject.h>

// Ossim headers have their own definition of NULL that conflicts with
// VICAR. So undef, and it will be redefined in the following header files
#undef NULL

extern "C" {
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <tcl.h>
#include "zvproto.h"
#include "carto/cartoLsqUtils.h" 
#include "carto/cartoStrUtils.h" 
#include "carto/shapefil.h"
}

#ifndef MAX
#define MAX(a,b) ((a)>(b)?(a):(b))
#define MIN(a,b) ((a)<(b)?(a):(b))
#endif

static int verifyFile( const char * path )
{
  struct stat statBuf;	

  if ( ! stat( path, & statBuf ) )
    return 1;
  else {
    fprintf( stderr, "failed opening input file %s\n", path );
    return 0;
  }
}

static int ossimGetEntryListSize( char * path ) {
  if ( ! verifyFile( path ) )
    return 0;

  ossimRefPtr<ossimImageHandler> ih = ossimImageHandlerRegistry::instance()->open( ossimFilename( path ) );

  if (! ih.valid())
    return 0;

  vector< ossim_uint32 > entryList;
  ih->getEntryList( entryList );

  return entryList.size();
}

static int ossimGetEntryListSizeProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"ossim::getEntryListSize pathToImgOrA.Toc\"";

  if (objc != 2) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * path = Tcl_GetStringFromObj( objv [1], 0 );

  if ( ! verifyFile( path ) ) {
    Tcl_SetResult( interp, const_cast<char*>( "error opening input file" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  Tcl_SetObjResult( interp, Tcl_NewIntObj( ossimGetEntryListSize( path ) ) );

  return TCL_OK;
}

static ossimRefPtr<ossimNitfRegisteredTag> getTag( const char * tagName, const char * path, int image, int whine = 0 ) {
  if ( ! verifyFile( path ) )
    return 0;

  ossimRefPtr<ossimNitfFile> nitfFile = new ossimNitfFile();
  nitfFile->parseFile( ossimFilename( path ) );

  ossimNitfTagInformation tagInfo;
  if ( nitfFile->getNewImageHeader( image )->getTagInformation( tagInfo, tagName ) ) {
    ossimRefPtr<ossimNitfRegisteredTag> regTag = ossimNitfTagFactoryRegistry::instance()->create( tagName );

    if ( regTag.valid() ) {
      ifstream inputStream( path );

      inputStream.seekg( tagInfo.getTagDataOffset() );
      regTag->parseStream( inputStream );

      nitfFile = 0;
      return regTag;

    } else
      if ( whine )
	cerr << "Invalid tag " << tagName << " for input " << path << endl;
  } else
    if ( whine )
      cerr << "GetTag returned NULL for tag " << tagName << " for input " << path << endl;

  nitfFile = 0;
  return 0;
}

// ossim::getTagValue /net/lorenz/export/data1/nitf/quickbird/03MAY05072951-P1BS-000000050393_01_P005.NTF 0 RPC00B
static int ossimGetTagValueProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"ossim::getTagValue pathToImgOrA.Toc index tag\"";

  if (objc != 4) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * path = strdup( Tcl_GetStringFromObj( objv [1], 0 ) );

  if ( ! verifyFile( path ) ) {
    Tcl_SetResult( interp, const_cast<char*>( "error opening input file" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * tagName = strdup( Tcl_GetStringFromObj( objv [3], 0 ) );
  int index;
  

  if ( Tcl_GetIntFromObj( interp, objv[ 2 ], & index) != TCL_OK ) {
    Tcl_SetResult (interp, const_cast<char*>( "failed to parse int from index" ), TCL_VOLATILE);
    free( path );
    return TCL_ERROR;
  }

  if ( index < 0 || index >= ossimGetEntryListSize( path ) ) {
    Tcl_SetResult (interp, const_cast<char*>( "bad image index, must be in range 0 .. entryListSize - 1" ), TCL_VOLATILE);
    free( path );
    return TCL_ERROR;
  }

  //ossimRefPtr<ossimNitfFile> nitfFile;
  //nitfFile->parseFile( ossimFilename( path ) );

  int returnValue = TCL_OK;
  ostringstream oss;

  ossimRefPtr<ossimNitfRegisteredTag> regTag = getTag( tagName, path, index );
 
  if ( ! regTag ) {
    Tcl_SetResult (interp, const_cast<char*>( "ossim::getTagValue: error: tag not found" ), TCL_VOLATILE);
    returnValue = TCL_ERROR;
  }  else {
    regTag->writeStream( oss );
    Tcl_SetResult (interp, (char *) (oss.str().c_str()), TCL_VOLATILE);
  }
    
  free( tagName );
  free( path );

  return returnValue;
}

// ossim::printTag /net/lorenz/export/data1/nitf/quickbird/03MAY05072951-P1BS-000000050393_01_P005.NTF 0 RPC00B
static int ossimPrintTagProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"ossim::printTag pathToImgOrA.Toc index tag\"";

  if (objc != 4) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * path = strdup( Tcl_GetStringFromObj( objv [1], 0 ) );

  if ( ! verifyFile( path ) ) {
    Tcl_SetResult( interp, const_cast<char*>( "error opening input file" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * tagName = strdup( Tcl_GetStringFromObj( objv [3], 0 ) );
  int index;
  

  if ( Tcl_GetIntFromObj( interp, objv[ 2 ], & index) != TCL_OK ) {
    Tcl_SetResult (interp, const_cast<char*>( "failed to parse int from index" ), TCL_VOLATILE);
    free( path );
    return TCL_ERROR;
  }

  if ( index < 0 || index >= ossimGetEntryListSize( path ) ) {
    Tcl_SetResult (interp, const_cast<char*>( "bad image index, must be in range 0 .. entryListSize - 1" ), TCL_VOLATILE);
    free( path );
    return TCL_ERROR;
  }

  int returnValue = TCL_OK;
  ostringstream oss;

  ossimRefPtr<ossimNitfRegisteredTag> regTag = getTag( tagName, path, index );
 
  if ( ! regTag ) {
    Tcl_SetResult (interp, const_cast<char*>( "ossim::getTagValue: error: tag not found" ), TCL_VOLATILE);
    returnValue = TCL_ERROR;
  }  else {
    regTag->print( oss, "" );
    Tcl_SetResult (interp, (char *) (oss.str().c_str()), TCL_VOLATILE);
  }
    
  free( tagName );
  free( path );

  return returnValue;
}

static int ossimGetEntryGeometryProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"ossim::getEntryGeometry pathToImgOrA.Toc index\"";

  if (objc != 3) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * path = strdup( Tcl_GetStringFromObj( objv [1], 0 ) );

  if ( ! verifyFile( path ) ) {
    Tcl_SetResult( interp, const_cast<char*>( "error opening input file" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  int index;

  if (Tcl_GetIntFromObj( interp, objv[ 2 ], & index) != TCL_OK ) {
    Tcl_SetResult (interp, const_cast<char*>( "failed to parse int from index" ), TCL_VOLATILE);
    free( path );
    return TCL_ERROR;
  }

  ossimRefPtr<ossimImageHandler> ih = ossimImageHandlerRegistry::instance()->open( ossimFilename( path ) );

  vector< ossim_uint32 > entryList;
  ih->getEntryList( entryList );

  ih->setCurrentEntry( entryList[ index ] );

  ossimKeywordlist keywordList;
  ossimImageGeometry * oig = ih->getImageGeometry();
  if ( oig && oig->saveState( keywordList ) ) {
    Tcl_SetObjResult( interp, Tcl_NewStringObj( (const char *) (keywordList.toString()), -1 ) );
    free( path );
    return TCL_OK;
  } else {
    Tcl_SetObjResult( interp, Tcl_NewStringObj( "Failed to get image geometry", -1 ) );
    cerr << "Failed to get image geometry" << endl;
    free( path );
    return TCL_ERROR;
  }
}

static int ossimGetNumberOfOutputBandsProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"ossim::getNumberOfOutputBands pathToImgOrA.Toc index\"";

  if (objc != 3) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * path = strdup( Tcl_GetStringFromObj( objv [1], 0 ) );

  if ( ! verifyFile( path ) ) {
    Tcl_SetResult( interp, const_cast<char*>( "error opening input file" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  int index;

  if (Tcl_GetIntFromObj( interp, objv[ 2 ], & index) != TCL_OK ) {
    Tcl_SetResult (interp, const_cast<char*>( "failed to parse int from index" ), TCL_VOLATILE);
    free( path );
    return TCL_ERROR;
  }

  ossimRefPtr<ossimImageHandler> ih = ossimImageHandlerRegistry::instance()->open( ossimFilename( path ) );

  vector< ossim_uint32 > entryList;
  ih->getEntryList( entryList );

  ih->setCurrentEntry( entryList[ index ] );

  ossim_uint32 bandCount = ih->getNumberOfOutputBands();

  Tcl_SetObjResult( interp, Tcl_NewIntObj( bandCount ) );

  free( path );

  return TCL_OK;
}

static int addStringToProperty (const char * name, const char * value, int unitCount, int * vunits, const char * property, int noQuotes) {
  int vunit;
  
  for (vunit = 0; vunit < unitCount; vunit ++)
    if (zladd (vunits [vunit], const_cast<char*>( "PROPERTY" ),
	       (char *) name, (char *) value,
	       const_cast<char*>( "PROPERTY" ), property,
	       const_cast<char*>( "FORMAT" ), const_cast<char*>( "STRING" ), NULL) != 1)
      return 1;
  return 0;
}

static int ossimRawToNitfProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  /* sl, ss are one-based */
  const char * usage = "wrong # args: should be \"ossim::rawToNitf in.ras out.ntf nl ns\"";

  if (objc != 5) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * inPath = strdup( Tcl_GetStringFromObj( objv [1], 0 ) );

  if ( ! verifyFile( inPath ) ) {
    Tcl_SetResult( interp, const_cast<char*>( "error opening input file" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * outPath = strdup( Tcl_GetStringFromObj( objv [2], 0 ) );
  char * nl = strdup( Tcl_GetStringFromObj( objv [3], 0 ) );
  char * ns = strdup( Tcl_GetStringFromObj( objv [4], 0 ) );

  // create omd file
  char * omdPath = (char *) malloc( strlen( inPath ) + 5 ); // plus .omd\0

  strcpy( omdPath, inPath );
  char * p = strrchr( omdPath, '.' );
  if ( p )
    * p = '\0';
  strcat( omdPath, ".omd" );

  FILE * f = fopen( omdPath, "w" );

  if ( ! f ) {
    fprintf( stderr, "fopen failed to create \"%s\"\n", omdPath );
    Tcl_SetResult( interp, const_cast<char*>( "ossim::rawToNitf failed to create omd file" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  fprintf( f, "filename:         %s\n", inPath );
  fprintf( f, "image_type:       general_raster_bil\n" );
  fprintf( f, "interleave_type:  bil\n" );
  fprintf( f, "number_bands:     1\n" );
  fprintf( f, "number_lines:     %s\n", nl );
  fprintf( f, "number_samples:   %s\n", ns );
  fprintf( f, "scalar_type:      sshort16\n" );
  fprintf( f, "band1.null_value: 0\n" );
  fprintf( f, "band1.min_value:  -32768\n" );
  fprintf( f, "band1.max_value:  32767\n" );

  fclose( f );

  // create nitf file

  ossimImageHandler * ih = ossimImageHandlerRegistry::instance()->open( ossimFilename( inPath ) );
  //ossimRefPtr<ossimImageHandler> ih = ossimImageHandlerRegistry::instance()->open( ossimFilename( inPath ) );

  ossimNitfWriter nw( outPath, ih );

  nw.execute();

  remove( omdPath );

  free( inPath );
  free( outPath );
  free( nl );
  free( ns );
  free( omdPath );

  return TCL_OK;
}

static char * getProp( const char * pName, const char * label, char * buf ) {
  const char * p1, * p2;

  if ( buf )
    free( buf );

  if ( ( p1 = strstr( label, pName ) ) && ( p1 += strlen( pName ) ) && ( p2 = strchr( p1 + 1, '\'' ) ) ) {
    p2 --;
    buf = (char *) malloc( p2 - p1 + 2 );
    strncpy( buf, p1, p2 - p1 + 1 );
    buf[ p2 - p1 + 1 ] = '\0';
  } else
    buf = 0;

  return buf;
}

// return 0 if invalid value
static int deg2dms( float f, char * latLonBuf, char latP ) {
  int neg = f < 0.0;

  if ( neg )
    f = - f;

  int deg = (int) f;
  int min = (int) ( ( f - deg ) * 60 );
  int sec = (int) ( ( f - deg - ( min / 60.0 ) ) * 3600 );

  if ( latP ) {
    if ( f <= 90.0 ) {
      sprintf( latLonBuf, "%02d%02d%02d%c", deg, min, sec, neg?'S':'N' );
      return 1;
    } else {
      sprintf( latLonBuf, "       " );
      return 0;
    }
  } else {
    if ( f <= 180.0 ) {
      sprintf( latLonBuf, "%03d%02d%02d%c", deg, min, sec, neg?'W':'E' );
      return 1;
    } else {
      sprintf( latLonBuf, "        " );
      return 0;
    }
  }
}

static double extractLat (const char * mag, const char * dir) {
  int deg, min, sec;
  double lat;
  char buf[10];

  if (*dir == 'N' || *dir == 'S') {
    sscanf (mag, "%02d%02d%02d", &deg, &min, &sec);
    if (*dir == 'N')
      lat = (double) deg + min / 60.0 + sec / 3600.0;
    else
      lat = - ((double) deg + min / 60.0 + sec / 3600.0);
  } else {			/* signed decimal degrees */
    strncpy(buf, mag, 7);
    buf[7] = '\0';

    sscanf(buf, "%lf", &lat);
  }

  return lat;
}

static double extractLon (const char * mag, const char * dir) {
  int deg, min, sec;
  double lon;
  char buf[10];

  if (*dir == 'E' || *dir == 'W') {
    sscanf (mag, "%03d%02d%02d", &deg, &min, &sec);
    if (*dir == 'E')
      lon = (double) deg + min / 60.0 + sec / 3600.0;
    else
      lon = - ((double) deg + min / 60.0 + sec / 3600.0);
  } else {			/* signed decimal degrees */
    strncpy(buf, mag, 8);
    buf[8] = '\0';
    sscanf(buf, "%lf", &lon);
  }

  return lon;
}

static double specConstrainedCoefficient( char rpcType, double d )
{
  if ( rpcType == 'a' ) {
    if ( fabs( d ) < 0.524287E-7 )
      d = 0.0;
  } else if ( rpcType == 'b' ) {
    if ( fabs( d ) < 9.999999E-9 )
      d = 0.0;
  } else			// unknown rpcType
    d = 0.0;

  return d;
}

static char* getLabel( char* path, Tcl_Interp *interp, int* maxNlNs, int* lblsizeP, int* nbP, int* nlP, int* nsP, char** fmtP, char** intfmtP ) {
  int nbpp = 0;
  FILE* f = fopen( path, "r" );

  if ( ! f ) {
    Tcl_SetResult( interp, const_cast<char*>( "ossim::vicarToNitf failed to open inp VICAR file" ), TCL_VOLATILE );
    return 0;
  }

  char buf[ 42 ];
  fgets( buf, 9, f );

  if ( strcmp( buf, "LBLSIZE=") ) {
    fclose( f );
    Tcl_SetResult( interp, const_cast<char*>( "ossim::vicarToNitf inp not a VICAR file" ), TCL_VOLATILE );
    return 0;
  }

  int i = 0;

  while ( i < 42 && fgets( buf + i, 2, f ) && buf[ i ] != ' ' )
    i++;

  int lblsize = 0;

  if ( sscanf( buf, "%d", & lblsize ) != 1 ) {
    fclose( f );
    Tcl_SetResult( interp, const_cast<char*>( "ossim::vicarToNitf error parsing LBLSIZE from inp VICAR file" ), TCL_VOLATILE );
    return 0;
  }

  if ( fseek( f, 0, SEEK_SET ) ) {
    fclose( f );
    Tcl_SetResult( interp, const_cast<char*>( "ossim::vicarToNitf error resetting fp in inp VICAR file" ), TCL_VOLATILE );
    return 0;
  }

  char * label = (char *) malloc( lblsize + 1 );
  label[ lblsize ] = '\0';
  if ( ! label ) {
    fclose( f );
    Tcl_SetResult( interp, const_cast<char*>( "ossim::vicarToNitf error mallocing label buffer for inp VICAR file" ), TCL_VOLATILE );
    return 0;
  }

  if ( fread( label, lblsize, 1, f ) != 1 ) {
    fclose( f );
    Tcl_SetResult( interp, const_cast<char*>( "ossim::vicarToNitf error reading label from inp VICAR file" ), TCL_VOLATILE );
    return 0;
  }

  char * p;

  p = strstr( label, " FORMAT=" );

  if ( ! p ) {
    Tcl_SetResult( interp, const_cast<char*>( "ossim::vicarToNitf error finding FORMAT in label in inp VICAR file" ), TCL_VOLATILE );
    return 0;
  }

  p += 8; /* skip " FORMAT=" */
  i = 0;

  while ( i < 42 && ( buf[ i ] = p[ i ] ) != ' ' )
    i ++;

  buf[ i ] = '\0';

  char * fmt = strdup( buf );

  buf[ i ] = ' ';

  if ( ! strcmp( fmt, "'BYTE'" ) )
    nbpp = 1;
  else if ( ! strcmp( fmt, "'HALF'" ) )
    nbpp = 2;
  else {
    printf( "FORMAT=%s\n", fmt );
    Tcl_SetResult( interp, const_cast<char*>( "ossim::vicarToNitf FORMAT in VICAR file label is neither BYTE nor HALF" ), TCL_VOLATILE );
    return 0;
  }

  p = strstr( label, " INTFMT=" );

  if ( ! p ) {
    Tcl_SetResult( interp, const_cast<char*>( "ossim::vicarToNitf error finding INTFMT in label in inp VICAR file" ), TCL_VOLATILE );
    return 0;
  }

  p += 8; /* skip " INTFMT=" */
  i = 0;

  while ( i < 42 && ( buf[ i ] = p[ i ] ) != ' ' )
    i ++;

  buf[ i ] = '\0';

  char * intfmt = strdup( buf );

  buf[ i ] = ' ';

  if ( strcmp( intfmt, "'LOW'" ) && strcmp( intfmt, "'HIGH'" ) ) {
    printf( "INTFMT=%s\n", intfmt );
    Tcl_SetResult( interp, const_cast<char*>( "ossim::vicarToNitf INTFMT in VICAR file label is neither LOW nor HIGH" ), TCL_VOLATILE );
    return 0;
  }

  // get NL
  p = strstr( label, " NL=" );

  if ( ! p ) {
    Tcl_SetResult( interp, const_cast<char*>( "ossim::vicarToNitf error finding NL in label from inp VICAR file" ), TCL_VOLATILE );
    return 0;
  }

  p += 4; /* skip " NL=" */
  i = 0;

  while ( i < 42 && ( buf[ i ] = p[ i ] ) != ' ' )
    i ++;

  int nl;

  if ( sscanf( buf, "%d", & nl ) != 1 ) {
    Tcl_SetResult( interp, const_cast<char*>( "ossim::vicarToNitf error parsing NL from inp VICAR file label" ), TCL_VOLATILE );
    return 0;
  }

  *maxNlNs = MAX( *maxNlNs, nl );

  // get NS
  p = strstr( label, " NS=" );

  if ( ! p ) {
    Tcl_SetResult( interp, const_cast<char*>( "ossim::vicarToNitf error finding NS in label from inp VICAR file" ), TCL_VOLATILE );
    return 0;
  }

  p += 4; /* skip " NS=" */
  i = 0;

  while ( i < 42 && ( buf[ i ] = p[ i ] ) != ' ' )
    i ++;

  int ns;

  if ( sscanf( buf, "%d", & ns ) != 1 ) {
    Tcl_SetResult( interp, const_cast<char*>( "ossim::vicarToNitf error parsing NS from inp VICAR file label" ), TCL_VOLATILE );
    return 0;
  }

  *maxNlNs = MAX( *maxNlNs, ns );

  // get NB
  p = strstr( label, " NB=" );

  if ( ! p ) {
    Tcl_SetResult( interp, const_cast<char*>( "ossim::vicarToNitf error finding NB in label from inp VICAR file" ), TCL_VOLATILE );
    return 0;
  }

  p += 4; /* skip " NB=" */
  i = 0;

  while ( i < 42 && ( buf[ i ] = p[ i ] ) != ' ' )
    i ++;

  int nb;

  if ( sscanf( buf, "%d", & nb ) != 1 ) {
    Tcl_SetResult( interp, const_cast<char*>( "ossim::vicarToNitf error parsing NB from inp VICAR file label" ), TCL_VOLATILE );
    return 0;
  }

  // Check for split label
  if ( strstr( label, " EOL=1" ) ) { // there is a split label
    long secondLabelLoc = 0;

    // if there is not a COMPRESS attribute, or if it's value is 'NONE'
    if ( ! strstr( label, " COMPRESS='" ) || strstr( label, " COMPRESS='NONE'" ) ) {

      secondLabelLoc = lblsize + nl * ns * nb * nbpp;

    } else {
      // else there is a COMPRESS attribute, and it's value is not 'NONE'
      Tcl_SetResult( interp, const_cast<char*>( "ossim::vicarToNitf: unable to process compressed inp VICAR file" ), TCL_VOLATILE );
      return 0;
    }

    if ( fseek( f, secondLabelLoc, SEEK_SET ) ) {
      fclose( f );
      Tcl_SetResult( interp, const_cast<char*>( "ossim::vicarToNitf error resetting fp to second label in inp VICAR file" ), TCL_VOLATILE );
      return 0;
    }

    fgets( buf, 9, f );

    if ( strcmp( buf, "LBLSIZE=") ) {
      fclose( f );
      Tcl_SetResult( interp, const_cast<char*>( "ossim::vicarToNitf second label not found" ), TCL_VOLATILE );
      return 0;
    }

    i = 0;

    while ( i < 42 && fgets( buf + i, 2, f ) && buf[ i ] != ' ' )
      i++;

    int lblsize2 = 0;
    
    if ( sscanf( buf, "%d", & lblsize2 ) != 1 ) {
      fclose( f );
      Tcl_SetResult( interp, const_cast<char*>( "ossim::vicarToNitf error parsing LBLSIZE2 from inp VICAR file" ), TCL_VOLATILE );
      return 0;
    }

    if ( fseek( f, secondLabelLoc, SEEK_SET ) ) {
      fclose( f );
      Tcl_SetResult( interp, const_cast<char*>( "ossim::vicarToNitf error resetting fp to second label in inp VICAR file" ), TCL_VOLATILE );
      return 0;
    }

    label = (char *) realloc( label, lblsize + lblsize2 + 1 );
    label[ lblsize + lblsize2 ] = '\0';

    if ( ! label ) {
      fclose( f );
      Tcl_SetResult( interp, const_cast<char*>( "ossim::vicarToNitf error reallocing label buffer for inp VICAR file" ), TCL_VOLATILE );
      return 0;
    }

    if ( fread( label + lblsize, lblsize2, 1, f ) != 1 ) {
      fclose( f );
      Tcl_SetResult( interp, const_cast<char*>( "ossim::vicarToNitf error reading second label from inp VICAR file" ), TCL_VOLATILE );
      return 0;
    }

    // replace nulls with spaces
    for ( i = 0; i < lblsize + lblsize2 ; i++ )
      if ( ! label[ i ] )
	label[ i ] = ' ';

  }

  fclose( f );

  *lblsizeP = lblsize;
  *nbP = nb;
  *nlP = nl;
  *nsP = ns;
  *fmtP = fmt;
  *intfmtP = intfmt;

  return label;
}

static int vicarGetLabelProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"vicar::getLabel img\"";

  if (objc != 2) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * path = strdup( Tcl_GetStringFromObj( objv [1], 0 ) );

  if ( ! verifyFile( path ) ) {
    Tcl_SetResult( interp, const_cast<char*>( "error opening input file" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * label = 0;
  int maxNlNs, lblsize, nb, nl, ns;
  char * fmt, * intfmt;
  
  if ( ! ( label = getLabel( path, interp, &maxNlNs, &lblsize, &nb, &nl, &ns, &fmt, &intfmt ) ) )
    return TCL_ERROR;

  free( fmt );
  free( intfmt );

  Tcl_SetResult( interp, label, TCL_VOLATILE );

  free( label );

  return TCL_OK;
}

static int ossimVicarToNitfInternal( int signedPixel, ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  /* sl, ss are one-based */
  const char * usage = "wrong # args: should be \"ossim::vicarToNitf in1.ras in2.ras ... inN.ras out.ntf\"";

  if (objc < 3) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  int maxNlNs = 0;
  int inCount = objc - 2;
  char ** inPaths = (char **) malloc (sizeof (char *) * inCount);
  char ** inLabels = (char **) malloc (sizeof (char *) * inCount);
  char ** omdPaths = (char **) malloc (sizeof (char *) * inCount);
  int lblsize = 0;
  int nb = 0;
  int nl = 0;
  int ns = 0;
  char * fmt = 0;
  char * intfmt = 0;
  for (int pathIndex = 0; pathIndex < objc - 2; pathIndex ++) {
    inPaths[ pathIndex ] = strdup( Tcl_GetStringFromObj( objv [ pathIndex + 1 ], 0 ) );

    if ( ! verifyFile( inPaths[ pathIndex ] ) ) {
      Tcl_SetResult( interp, const_cast<char*>( "error opening input file" ), TCL_VOLATILE );
      return TCL_ERROR;
    }

    if ( ! ( inLabels[ pathIndex ] = getLabel( inPaths[ pathIndex ], interp, &maxNlNs, &lblsize, &nb, &nl, &ns, &fmt, &intfmt ) ) )
      return TCL_ERROR;
  }
  char * outPath = strdup( Tcl_GetStringFromObj( objv [ objc - 1 ], 0 ) );

  FILE * f;

  // iterate through input files creating omd files for them
  for (int pathIndex = 0; pathIndex < inCount; pathIndex ++) {
    // create omd file
    //char * omdPath = (char *) malloc( strlen( inPaths[ pathIndex ] ) + 5 ); // plus .omd\0
    omdPaths[ pathIndex ] = (char *) malloc( strlen( inPaths[ pathIndex ] ) + 5 ); // plus .omd\0
    char * omdPath = omdPaths[ pathIndex ];

    strcpy( omdPath, inPaths[ pathIndex ] );
    char * p = strrchr( omdPath, '.' );
    if ( p )
      * p = '\0';
    strcat( omdPath, ".omd" );

    f = fopen( omdPath, "w" );

    if ( ! f ) {
      fprintf( stderr, "fopen failed to create \"%s\"\n", omdPath );
      Tcl_SetResult( interp, const_cast<char*>( "ossim::vicarToNitf failed to create omd file" ), TCL_VOLATILE );
      return TCL_ERROR;
    }

    fprintf( f, "filename:         %s\n", inPaths[ pathIndex ] );
    fprintf( f, "header_size:      %d\n", lblsize );
    fprintf( f, "image_type:       general_raster_bsq\n" );
    fprintf( f, "interleave_type:  bsq\n" );
    fprintf( f, "number_bands:     %d\n", nb );
    fprintf( f, "number_lines:     %d\n", nl );
    fprintf( f, "number_samples:   %d\n", ns );

    if ( ! strcmp( fmt, "'HALF'" ) ) {
      if ( signedPixel )
	fprintf( f, "scalar_type:      sshort16\n" );
      else
	fprintf( f, "scalar_type:      ushort16\n" );
      // it's either big or little
      if ( ! strcmp( intfmt, "'LOW'" ) ) // if it's little
	fprintf( f, "byte_order:       little_endian\n" );
      else				// else it's big
	fprintf( f, "byte_order:       big_endian\n" );
      for ( int bandNum = 1; bandNum <= nb; bandNum ++ ) {
	fprintf( f, "band%d.null_value: 0\n", bandNum );
	fprintf( f, "band%d.min_value:  0\n", bandNum );
	fprintf( f, "band%d.max_value:  65535\n", bandNum );
      }
//       fprintf( f, "band1.null_value: 0\n" );
//       fprintf( f, "band1.min_value:  0\n" );
//       fprintf( f, "band1.max_value:  65535\n" );
    } else {			// 'BYTE'
      fprintf( f, "scalar_type:      uchar\n" );
      for ( int bandNum = 1; bandNum <= nb; bandNum ++ ) {
	fprintf( f, "band%d.null_value: 0\n", bandNum );
	fprintf( f, "band%d.min_value:  0\n", bandNum );
	fprintf( f, "band%d.max_value:  255\n", bandNum );
      }
    }

    fclose( f );
  }

  // create nitf file (omd file must exist first)
  ossimImageHandler * ih = ossimImageHandlerRegistry::instance()->open( ossimFilename( inPaths[ 0 ] ) );
  //ossimRefPtr<ossimImageHandler> ih = ossimImageHandlerRegistry::instance()->open( ossimFilename( inPaths[ 0 ] ) );

  ossimNitfWriter nw( outPath, ih );

  ossimRefPtr<ossimProperty> bp = new ossimBooleanProperty(ossimKeywordNames::CREATE_EXTERNAL_GEOMETRY_KW, false);

  nw.setProperty(bp);

  nw.pushBackImageHandler( ih );

  for (int pathIndex = 1; pathIndex < inCount; pathIndex ++)
    nw.pushBackImageHandler( ossimImageHandlerFactory::instance()->open( ossimFilename( inPaths[ pathIndex ] ) ) );

  ossimContainerProperty * fileHeader = new ossimContainerProperty( "file_header" ); // from ossimNitfWriter.cpp:151

  // iterate through input files
  for (int pathIndex = 0; pathIndex < inCount; pathIndex ++) {
    char * label = inLabels[ pathIndex ];

    // create omd file
    //char * omdPath = (char *) malloc( strlen( inPaths[ pathIndex ] ) + 5 ); // plus .omd\0
    omdPaths[ pathIndex ] = (char *) malloc( strlen( inPaths[ pathIndex ] ) + 5 ); // plus .omd\0
    char * omdPath = omdPaths[ pathIndex ];

    strcpy( omdPath, inPaths[ pathIndex ] );
    char * p = strrchr( omdPath, '.' );
    if ( p )
      * p = '\0';
    strcat( omdPath, ".omd" );

    ossimContainerProperty * imageHeader = 0;

    if (pathIndex == 0) {
      imageHeader = new ossimContainerProperty( "image_header" ); // from ossimNitfWriter.cpp:162
    } else {
      imageHeader = new ossimContainerProperty( "secondary_image_header" );
    }

    {
      char intBuf[ 20 ];

      sprintf( intBuf, "%d", pathIndex + 1 ); // pathIndex is zero-based; idlvl is one-based
      ossimStringProperty * idlvl = new ossimStringProperty( "idlvl" ); // from ossimNitfImageHeaderV2_X.cpp:39
      idlvl->setValue( intBuf );
      imageHeader->addChild( idlvl );
    }

  //    {
  //      ossimNitfUse00aTag * use00aTag = new ossimNitfUse00aTag;
  //      use00aTag->setAngleToNorth("42");
  //      ossimRefPtr<ossimNitfRegisteredTag> registeredTag = use00aTag;
  //      nw.addRegisteredTag(registeredTag);
  //    }

    {
      // expecting seed like 1, 2, 3, ...
      char * use26aSeed = getenv("use26aSeed");
      if ( use26aSeed ) {
	int seed = atoi( use26aSeed );
	char buf[30];
      
	ossimNitfUse26aTag * use26aTag = new ossimNitfUse26aTag;
	sprintf( buf, "%d",  seed + 36 );
	use26aTag->setMeanGsd( buf );
	sprintf( buf, "%1.2f", (float) seed / 100 );
	use26aTag->setOblAng( buf );
	sprintf( buf, "%1.2f", (float) seed / 10 );
	use26aTag->setRollAng( buf );
	ossimRefPtr<ossimNitfRegisteredTag> registeredTag = use26aTag;
	nw.addRegisteredTag(registeredTag);
      }
    }

    {
      typedef ossimNitfImageBandV2_1* bandInfoPtr;
      bandInfoPtr* bandInfos = new bandInfoPtr[nb];
      for ( int band = 0; band < nb; band ++ )
	bandInfos[band] = new ossimNitfImageBandV2_1();
      char * buf = 0;
      char igeoloBuf[ 61 ];
      char latLonBuf[ 10 ];
      float f;
      int gotCoord = 1;		// directed by customer to force igeolo field, even if blank

      igeoloBuf[ 0 ] = '\0';

      if ( ( buf = getProp( "NITF_CORNERLAT1='", label, buf ) ) ) {
	sscanf( buf, "%f", &f );
	//sprintf( latLonBuf, "%+07.3f", f );
	gotCoord |= deg2dms( f, latLonBuf, 1 );
	strcat( igeoloBuf, latLonBuf );
      }

      if ( ( buf = getProp( "NITF_CORNERLON1='", label, buf ) ) ) {
	sscanf( buf, "%f", &f );
	//sprintf( latLonBuf, "%+08.3f", f );
	gotCoord |= deg2dms( f, latLonBuf, 0 );
	strcat( igeoloBuf, latLonBuf );
      }

      if ( ( buf = getProp( "NITF_CORNERLAT2='", label, buf ) ) ) {
	sscanf( buf, "%f", &f );
	//sprintf( latLonBuf, "%+07.3f", f );
	gotCoord |= deg2dms( f, latLonBuf, 1 );
	strcat( igeoloBuf, latLonBuf );
      }

      if ( ( buf = getProp( "NITF_CORNERLON2='", label, buf ) ) ) {
	sscanf( buf, "%f", &f );
	//sprintf( latLonBuf, "%+08.3f", f );
	gotCoord |= deg2dms( f, latLonBuf, 0 );
	strcat( igeoloBuf, latLonBuf );
      }

      if ( ( buf = getProp( "NITF_CORNERLAT3='", label, buf ) ) ) {
	sscanf( buf, "%f", &f );
	//sprintf( latLonBuf, "%+07.3f", f );
	gotCoord |= deg2dms( f, latLonBuf, 1 );
	strcat( igeoloBuf, latLonBuf );
      }

      if ( ( buf = getProp( "NITF_CORNERLON3='", label, buf ) ) ) {
	sscanf( buf, "%f", &f );
	//sprintf( latLonBuf, "%+08.3f", f );
	gotCoord |= deg2dms( f, latLonBuf, 0 );
	strcat( igeoloBuf, latLonBuf );
      }

      if ( ( buf = getProp( "NITF_CORNERLAT4='", label, buf ) ) ) {
	sscanf( buf, "%f", &f );
	//sprintf( latLonBuf, "%+07.3f", f );
	gotCoord |= deg2dms( f, latLonBuf, 1 );
	strcat( igeoloBuf, latLonBuf );
      }

      if ( ( buf = getProp( "NITF_CORNERLON4='", label, buf ) ) ) {
	sscanf( buf, "%f", &f );
	//sprintf( latLonBuf, "%+08.3f", f );
	gotCoord |= deg2dms( f, latLonBuf, 0 );
	strcat( igeoloBuf, latLonBuf );
      }

#ifdef INCLUDE_AFIDS_A
#include <ossim/support_data/afids_a_4.h>
      {
#endif

	if ( inCount == 1 && nb == 3 ) { // multi-file or non-three-band
	  for ( int band = 0; band < nb; ++ band ) {
	    switch ( band ) {
	    case 0:
	      bandInfos[band]->setBandRepresentation( "R" );
	      bandInfos[band]->setBandSignificance( "      " );
	      break;
	    case 1:
	      bandInfos[band]->setBandRepresentation( "G" );
	      bandInfos[band]->setBandSignificance( "      " );
	      break;
	    case 2:
	      bandInfos[band]->setBandRepresentation( "B" );
	      bandInfos[band]->setBandSignificance( "      " );
	      break;
	    }
	  }
	} else {		//  no TAG, multi-file or non-three-band
	  for ( int band = 0; band < nb; ++ band ) {
	    bandInfos[band]->setBandRepresentation( "M" );
	    bandInfos[band]->setBandSignificance( "      " );
	  }
	}

#ifdef INCLUDE_AFIDS_A
      }
#endif

      for ( int band = 0; band < nb; ++ band )
	nw.pushBackImageHeaderBandInfo( bandInfos[band], pathIndex );

      if (pathIndex == 0) {
	nw.setProperty( imageHeader );
      } else {
	nw.setSecondaryImageProperty( imageHeader, pathIndex );
      }
    }

    {
      ossimRefPtr< ossimNitfRpcBase > rpcTag;
      char * buf = 0;
      int i;
      double d;
      char fmtBuf[ 50 ];
      char rpcType = 'x';

#ifdef INCLUDE_AFIDS_A
#include <ossim/support_data/afids_a_5.h>
#endif
	if ( ( buf = getProp( "NITF_CETAG='", label, buf ) )  &&
	   ! strcmp( buf, "RPC00A" ) ) {
	rpcTag = new ossimNitfRpcATag;
	rpcType = 'a';
      } else {
	rpcTag = new ossimNitfRpcBTag;
	rpcType = 'b';
      }

      rpcTag->setSuccess( true );
      if ( ( buf = getProp( "RPC_FIELD1='", label, buf ) ) ) {
	if ( *buf == '1' )
	  rpcTag->setSuccess( true );
	else
	  rpcTag->setSuccess( false );
      }

      if ( ( buf = getProp( "RPC_FIELD2='", label, buf ) ) ) {
	d = 0.0;
	sscanf( buf, "%lf", & d );
	sprintf( fmtBuf, "%07.2lf", d );
	rpcTag->setErrorBias( fmtBuf );
      } else
	  rpcTag->setSuccess( false );

      if ( ( buf = getProp( "RPC_FIELD3='", label, buf ) ) ) {
	d = 0.0;
	sscanf( buf, "%lf", & d );
	sprintf( fmtBuf, "%07.2lf", d );
	rpcTag->setErrorRand( fmtBuf );
      }
      if ( ( buf = getProp( "RPC_FIELD4='", label, buf ) ) ) {
	d = 0.0;
	i = 0;
	sscanf( buf, "%lf", & d );
	i = (int) d;
	sprintf( fmtBuf, "%06d", i );
	rpcTag->setLineOffset( fmtBuf );
      }
      if ( ( buf = getProp( "RPC_FIELD5='", label, buf ) ) ) {
	d = 0.0;
	i = 0;
	sscanf( buf, "%lf", & d );
	i = (int) d;
	sprintf( fmtBuf, "%05d", i );
	rpcTag->setSampleOffset( fmtBuf );
      }
      if ( ( buf = getProp( "RPC_FIELD6='", label, buf ) ) ) {
	d = 0.0;
	sscanf( buf, "%lf", & d );
	sprintf( fmtBuf, "%+08.4lf", d );
	rpcTag->setGeodeticLatOffset( fmtBuf );
      }
      if ( ( buf = getProp( "RPC_FIELD7='", label, buf ) ) ) {
	d = 0.0;
	sscanf( buf, "%lf", & d );
	sprintf( fmtBuf, "%+09.4lf", d );
	rpcTag->setGeodeticLonOffset( fmtBuf );
      }
      if ( ( buf = getProp( "RPC_FIELD8='", label, buf ) ) ) {
	d = 0.0;
	i = 0;
	sscanf( buf, "%lf", & d );
	i = (int) d;
	sprintf( fmtBuf, "%+05d", i );
	rpcTag->setGeodeticHeightOffset( fmtBuf );
      }
      if ( ( buf = getProp( "RPC_FIELD9='", label, buf ) ) ) {
	d = 0.0;
	i = 0;
	sscanf( buf, "%lf", & d );
	i = (int) d;
	sprintf( fmtBuf, "%06d", i );
	rpcTag->setLineScale( fmtBuf );
      }
      if ( ( buf = getProp( "RPC_FIELD10='", label, buf ) ) ) {
	d = 0.0;
	i = 0;
	sscanf( buf, "%lf", & d );
	i = (int) d;
	sprintf( fmtBuf, "%05d", i );
	rpcTag->setSampleScale( fmtBuf );
      }
      if ( ( buf = getProp( "RPC_FIELD11='", label, buf ) ) ) {
	d = 0.0;
	sscanf( buf, "%lf", & d );
	sprintf( fmtBuf, "%+08.4lf", d );
	rpcTag->setGeodeticLatScale( fmtBuf );
      }
      if ( ( buf = getProp( "RPC_FIELD12='", label, buf ) ) ) {
	d = 0.0;
	sscanf( buf, "%lf", & d );
	sprintf( fmtBuf, "%+09.4lf", d );
	rpcTag->setGeodeticLonScale( fmtBuf );
      }
      if ( ( buf = getProp( "RPC_FIELD13='", label, buf ) ) ) {
	d = 0.0;
	i = 0;
	sscanf( buf, "%lf", & d );
	i = (int) d;
	sprintf( fmtBuf, "%+05d", i );
	rpcTag->setGeodeticHeightScale( fmtBuf );
      }

      char nameBuf[ 20 ];

      for (int i = 1; i <= 20; i ++) {
	sprintf( nameBuf, "RPC_FIELD14%d='", i );
	if ( ( buf = getProp( nameBuf, label, buf ) ) ) {
	  d = 0.0;
	  sscanf( buf, "%lf", & d );
	  d = specConstrainedCoefficient( rpcType, d );
	  sprintf( fmtBuf, "%+10.6lE", d );
	  fmtBuf[11] = fmtBuf[12];
	  fmtBuf[12] = '\0';
	  rpcTag->setLineNumeratorCoeff( i - 1, fmtBuf );
	}
      }

      for (int i = 1; i <= 20; i ++) {
	sprintf( nameBuf, "RPC_FIELD15%d='", i );
	if ( ( buf = getProp( nameBuf, label, buf ) ) ) {
	  d = 0.0;
	  sscanf( buf, "%lf", & d );
	  d = specConstrainedCoefficient( rpcType, d );
	  sprintf( fmtBuf, "%+10.6lE", d );
	  fmtBuf[11] = fmtBuf[12];
	  fmtBuf[12] = '\0';
	  rpcTag->setLineDenominatorCoeff( i - 1, fmtBuf );
	}
      }

      for (int i = 1; i <= 20; i ++) {
	sprintf( nameBuf, "RPC_FIELD16%d='", i );
	if ( ( buf = getProp( nameBuf, label, buf ) ) ) {
	  d = 0.0;
	  sscanf( buf, "%lf", & d );
	  d = specConstrainedCoefficient( rpcType, d );
	  sprintf( fmtBuf, "%+10.6lE", d );
	  fmtBuf[11] = fmtBuf[12];
	  fmtBuf[12] = '\0';
	  rpcTag->setSampleNumeratorCoeff( i - 1, fmtBuf );
	}
      }

      for (int i = 1; i <= 20; i ++) {
	sprintf( nameBuf, "RPC_FIELD17%d='", i );
	if ( ( buf = getProp( nameBuf, label, buf ) ) ) {
	  d = 0.0;
	  sscanf( buf, "%lf", & d );
	  d = specConstrainedCoefficient( rpcType, d );
	  sprintf( fmtBuf, "%+10.6lE", d );
	  fmtBuf[11] = fmtBuf[12];
	  fmtBuf[12] = '\0';
	  rpcTag->setSampleDenominatorCoeff( i - 1, fmtBuf );
	}
      }

      ossimRefPtr<ossimNitfRegisteredTag> registeredTag = rpcTag.get();
      nw.addRegisteredTag(registeredTag, pathIndex);
    }

#ifdef INCLUDE_AFIDS_A
#include <ossim/support_data/afids_a_6.h>
#endif

    free( label );
  }

  ossimStringProperty * fbkgc = new ossimStringProperty( "fbkgc" ); // from ossimNitfFileHeaderV2_1.cpp:61
  fbkgc->setValue( "~~~" );	// ascii 126 126 126, an rgb triple for gray
  fileHeader->addChild( fbkgc );

  ossimStringProperty * fsclas = new ossimStringProperty( ossimNitfFileHeaderV2_X::FSCLAS_KW );
  fsclas->setValue( "U" );
  fileHeader->addChild( fsclas );

  ossimStringProperty * fscop = new ossimStringProperty( ossimNitfFileHeaderV2_X::FSCOP_KW );
  fscop->setValue( "00000" );
  fileHeader->addChild( fscop );

  ossimStringProperty * fscpys = new ossimStringProperty( ossimNitfFileHeaderV2_X::FSCPYS_KW );
  fscpys->setValue( "00000" );
  fileHeader->addChild( fscpys );

  ossimStringProperty * encryp = new ossimStringProperty( ossimNitfFileHeaderV2_X::ENCRYP_KW );
  encryp->setValue( "0" );
  fileHeader->addChild( encryp );

  ossimStringProperty * clevel = new ossimStringProperty( ossimNitfFileHeaderV2_X::CLEVEL_KW );
  if ( maxNlNs > 2048 )
    clevel->setValue( "05" );
  else
    clevel->setValue( "03" );
  fileHeader->addChild( clevel );

  nw.setProperty( fileHeader );

  nw.execute();

  for (int pathIndex = 0; pathIndex < inCount; pathIndex ++) {
    remove( omdPaths[ pathIndex ] );

    free( omdPaths[ pathIndex ] );
    free( inPaths[ pathIndex ] );
  }
  free( inPaths );
  free( omdPaths );
  free( outPath );

  return TCL_OK;
}

static int ossimVicarToNitfIntProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  return ossimVicarToNitfInternal( 0, clientData, interp, objc, objv );
}

static int ossimVicarToNitfSiProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  return ossimVicarToNitfInternal( 1, clientData, interp, objc, objv );
}

static int getTags( int openVunit, char * nitfPath, int image ) {
  int status = 0;
  ossimString oStr;
  
  // check for RPC tags
  ossimRefPtr<ossimNitfRegisteredTag> regTag = getTag( "RPC00A", nitfPath, image );
  if ( regTag.valid() )
      status |= addStringToProperty ("NITF_CETAG", "RPC00A", 1, & openVunit, "GEOTIFF", 0);
  else {
    regTag = getTag( "RPC00B", nitfPath, image );
    if ( regTag.valid() )
      status |= addStringToProperty ("NITF_CETAG", "RPC00B", 1, & openVunit, "GEOTIFF", 0);
  }

  if ( regTag.valid() ) {
    ossimNitfRegisteredTag * regTagp = regTag.get();
    ossimNitfRpcBase * rpcTag = (ossimNitfRpcBase *) regTagp;

    status = 0;
    if ( rpcTag->getSuccess() )
      status |= addStringToProperty ("RPC_FIELD1", "1", 1, & openVunit, "GEOTIFF", 0);
    else
      status |= addStringToProperty ("RPC_FIELD1", "0", 1, & openVunit, "GEOTIFF", 0);
    
    oStr = rpcTag->getErrorBias();
    status |= addStringToProperty ("RPC_FIELD2", oStr, 1, & openVunit, "GEOTIFF", 0);
    oStr = rpcTag->getErrorRand();
    status |= addStringToProperty ("RPC_FIELD3", oStr, 1, & openVunit, "GEOTIFF", 0);
    oStr = rpcTag->getLineOffset();
    status |= addStringToProperty ("RPC_FIELD4", oStr, 1, & openVunit, "GEOTIFF", 0);
    oStr = rpcTag->getSampleOffset();
    status |= addStringToProperty ("RPC_FIELD5", oStr, 1, & openVunit, "GEOTIFF", 0);
    oStr = rpcTag->getGeodeticLatOffset();
    status |= addStringToProperty ("RPC_FIELD6", oStr, 1, & openVunit, "GEOTIFF", 0);
    oStr = rpcTag->getGeodeticLonOffset();
    status |= addStringToProperty ("RPC_FIELD7", oStr, 1, & openVunit, "GEOTIFF", 0);
    oStr = rpcTag->getGeodeticHeightOffset();
    status |= addStringToProperty ("RPC_FIELD8", oStr, 1, & openVunit, "GEOTIFF", 0);
    oStr = rpcTag->getLineScale();
    status |= addStringToProperty ("RPC_FIELD9", oStr, 1, & openVunit, "GEOTIFF", 0);
    oStr = rpcTag->getSampleScale();
    status |= addStringToProperty ("RPC_FIELD10", oStr, 1, & openVunit, "GEOTIFF", 0);
    oStr = rpcTag->getGeodeticLatScale();
    status |= addStringToProperty ("RPC_FIELD11", oStr, 1, & openVunit, "GEOTIFF", 0);
    oStr = rpcTag->getGeodeticLonScale();
    status |= addStringToProperty ("RPC_FIELD12", oStr, 1, & openVunit, "GEOTIFF", 0);
    oStr = rpcTag->getGeodeticHeightScale();
    status |= addStringToProperty ("RPC_FIELD13", oStr, 1, & openVunit, "GEOTIFF", 0);
    char fieldName[ 30 ];
    for ( int i = 0; i < 20; ++ i ) {
      oStr = rpcTag->getLineNumeratorCoeff(i);
      sprintf( fieldName, "RPC_FIELD14%d", i + 1 );
      status |= addStringToProperty (fieldName, oStr, 1, & openVunit, "GEOTIFF", 0);
    }
    for ( int i = 0; i < 20; ++ i ) {
      oStr = rpcTag->getLineDenominatorCoeff(i);
      sprintf( fieldName, "RPC_FIELD15%d", i + 1 );
      status |= addStringToProperty (fieldName, oStr, 1, & openVunit, "GEOTIFF", 0);
    }
    for ( int i = 0; i < 20; ++ i ) {
      oStr = rpcTag->getSampleNumeratorCoeff(i);
      sprintf( fieldName, "RPC_FIELD16%d", i + 1 );
      status |= addStringToProperty (fieldName, oStr, 1, & openVunit, "GEOTIFF", 0);
    }
    for ( int i = 0; i < 20; ++ i ) {
      oStr = rpcTag->getSampleDenominatorCoeff(i);
      sprintf( fieldName, "RPC_FIELD17%d", i + 1 );
      status |= addStringToProperty (fieldName, oStr, 1, & openVunit, "GEOTIFF", 0);
    }

    if ( status ) {
      cerr << "addStringToProperty failed on RPCs" << endl;
      return 1;
    }
  } // done with RPCs

  // check for USE26A tag
  regTag = getTag( "USE26A", nitfPath, image );
  if ( regTag.valid() ) {
    ossimNitfRegisteredTag * regTagp = regTag.get();
    ossimNitfUse26aTag * use26aTag = (ossimNitfUse26aTag *) regTagp;

    status = 0;

    oStr = use26aTag->getMeanGsd();
    status |= addStringToProperty ("NITF_USE26A_MEAN_GSD", oStr, 1, & openVunit, "GEOTIFF", 0);
    oStr = use26aTag->getOblAng();
    status |= addStringToProperty ("NITF_USE26A_OBL_ANG", oStr, 1, & openVunit, "GEOTIFF", 0);
    oStr = use26aTag->getRollAng();
    status |= addStringToProperty ("NITF_USE26A_ROLL_ANG", oStr, 1, & openVunit, "GEOTIFF", 0);

    if ( status ) {
      cerr << "addStringToProperty failed on USE26A tag" << endl;
      return 1;
    }
  } // done with USE26A tag

#ifdef INCLUDE_AFIDS_A
#include <ossim/support_data/afids_a_7.h>
#endif

  // get image header data
  ossimRefPtr<ossimNitfFile> nitfFile = new ossimNitfFile();
  nitfFile->parseFile( ossimFilename( nitfPath ) );

  //ossimNitfImageHeaderV2_X* header = nitfFile->getNewImageHeader( 0 );
  ossimRefPtr<ossimNitfImageHeader> hdr = nitfFile->getNewImageHeader( 0 );
  ossimNitfImageHeaderV2_X* hdr2x = PTR_CAST(ossimNitfImageHeaderV2_X, hdr.get());
  if (hdr2x) {
    status |= addStringToProperty ("NITF_IMAGEDATIM", hdr2x->getAquisitionDateTime(), 1, & openVunit, "GEOTIFF", 0);
    oStr = hdr2x->getGeographicLocation();
  } else
    status = 1;

  const char * chars = oStr.chars();

  char doublePrintBuf [30];
  sprintf( doublePrintBuf, "%.15lf", extractLat (&chars[0],  &chars[6]));
  status |= addStringToProperty ("NITF_CORNERLAT1", doublePrintBuf, 1, & openVunit, "GEOTIFF", 0);
  sprintf( doublePrintBuf, "%.15lf", extractLon (&chars[7],  &chars[14]));
  status |= addStringToProperty ("NITF_CORNERLON1", doublePrintBuf, 1, & openVunit, "GEOTIFF", 0);
  sprintf( doublePrintBuf, "%.15lf", extractLat (&chars[15],  &chars[21]));
  status |= addStringToProperty ("NITF_CORNERLAT2", doublePrintBuf, 1, & openVunit, "GEOTIFF", 0);
  sprintf( doublePrintBuf, "%.15lf", extractLon (&chars[22],  &chars[29]));
  status |= addStringToProperty ("NITF_CORNERLON2", doublePrintBuf, 1, & openVunit, "GEOTIFF", 0);
  sprintf( doublePrintBuf, "%.15lf", extractLat (&chars[30],  &chars[36]));
  status |= addStringToProperty ("NITF_CORNERLAT3", doublePrintBuf, 1, & openVunit, "GEOTIFF", 0);
  sprintf( doublePrintBuf, "%.15lf", extractLon (&chars[37],  &chars[44]));
  status |= addStringToProperty ("NITF_CORNERLON3", doublePrintBuf, 1, & openVunit, "GEOTIFF", 0);
  sprintf( doublePrintBuf, "%.15lf", extractLat (&chars[45],  &chars[51]));
  status |= addStringToProperty ("NITF_CORNERLAT4", doublePrintBuf, 1, & openVunit, "GEOTIFF", 0);
  sprintf( doublePrintBuf, "%.15lf", extractLon (&chars[52],  &chars[59]));
  status |= addStringToProperty ("NITF_CORNERLON4", doublePrintBuf, 1, & openVunit, "GEOTIFF", 0);

  nitfFile = 0;

  if ( status ) {
    cerr << "addStringToProperty failed on image header" << endl;
    return 1;
  }

  return 0;
} 

// This was lifted from an old version of base/ossimCommon.cpp and is beyond deprecated. It
// should be updated to the current api.
ossim_uint32 ossimGetScalarSizeInBytes(ossimScalarType scalarType)
{
   switch(scalarType)
   {
      case OSSIM_UINT8:
      {
         return sizeof(ossim_uint8);
      }
      case OSSIM_SINT8:
      {
         return sizeof(ossim_sint8);
      }
      case OSSIM_UINT16:
      case OSSIM_USHORT11:
      {
         return sizeof(ossim_uint16);
      }
      case OSSIM_SINT16:
      {
         return sizeof(ossim_sint16);
      }
      case OSSIM_UINT32:
      {
         return sizeof(ossim_uint32);
      }
      case OSSIM_SINT32:
      {
         return sizeof(ossim_sint32);
      }
      case OSSIM_FLOAT32:
      case OSSIM_NORMALIZED_FLOAT:
      {
         return sizeof(ossim_float32);
      }
      case OSSIM_FLOAT64:
      case OSSIM_NORMALIZED_DOUBLE:
      {
         return sizeof(ossim_float64);
      }
      case OSSIM_SCALAR_UNKNOWN:
      default:
      {
            ossimNotify(ossimNotifyLevel_DEBUG)
               << __FILE__ << ":" << __LINE__
               << "\nUnhandled scalar type:  " << scalarType << std::endl;
         break;
      }
  }
  
  return 1;
}

/* three tiepoints in, labelstr receives transformation when rotated, scale when not rotated */
/* labelstr must be large enough to hold the label data (at least 500) */
static int isRotated( int* lines, int* samps, double* lats, double* lons, char* labelstr )
{
  int tiepoint;
  double img1[9], img2[9], map[6];
  double xmain, xcross, xtot;
  int scaletype;
  int ier;

  strcpy( labelstr, "");

  for ( tiepoint = 0; tiepoint < 3; tiepoint++ )
    {
      img1[ tiepoint ] = samps[ tiepoint ];
      img1[ tiepoint + 3 ] = lines[ tiepoint ];
      img1[ tiepoint + 6 ] = 1.0;
      img2[ tiepoint ] = img1[ tiepoint ];
      img2[ tiepoint + 3 ] = img1[ tiepoint + 3 ];
      img2[ tiepoint + 6 ] = 1.0;

      map[ tiepoint ] = lons[ tiepoint ];
      map[ tiepoint + 3] = lats[ tiepoint ];
    }

  dgauss(img1,map,3,1.e-14,&ier);
  if (ier!=0) {
    fprintf( stderr, "Tiepoints collinear" );
    return 0;
  }

  dgauss(img2,&map[3],3,1.e-14,&ier);
  if (ier!=0) {
    fprintf( stderr, "Tiepoints collinear" );
    return 0;
  }

  xmain = fabs( map[ 0 ] ) + fabs( map[ 4 ] );
  xcross = fabs( map[ 1 ] ) + fabs( map[ 3 ] );
  xtot = xmain+xcross;
  scaletype = xcross/xtot<1.e-10;

  if ( scaletype )
    scalefmt( labelstr, map[ 0 ], -map[ 4 ] );
  else
    trnsfmt( labelstr, map );

  return !scaletype;
}

static int ossimToVicarProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  /* sl, ss are one-based */
  const char * usage = "wrong # args: should be \"ossim::toVicar pathToImgOrA.Toc out.img [image band] [sl ss nl ns] [NL NS n e w s]\"";

  char tiePoint[100];
  char scale[100];
  char sourceNlStr[20], sourceNsStr[20];
  int stripLoaded = -1;
  ossimRefPtr< ossimImageData > imageData;
  ossimIrect * oneStrip = 0;
  int strip;
  int line;
  int lry;

  // process parameters (see usage string above)
  if (objc != 15 && objc != 9 && objc != 5 && objc != 3) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * inPath = strdup( Tcl_GetStringFromObj( objv [1], 0 ) );

  if ( ! verifyFile( inPath ) ) {
    Tcl_SetResult( interp, const_cast<char*>( "error opening input file" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * outPath = strdup( Tcl_GetStringFromObj( objv [2], 0 ) );
  int image, band, sl=1, ss=1, nl=0, ns=0;
  
  if (objc > 3 ) {
    if (Tcl_GetIntFromObj( interp, objv [3], & image ) != TCL_OK ||
	Tcl_GetIntFromObj( interp, objv [4], & band ) != TCL_OK ||
	(objc == 9 &&
	 (Tcl_GetIntFromObj( interp, objv [5], & sl ) != TCL_OK ||
	  Tcl_GetIntFromObj( interp, objv [6], & ss ) != TCL_OK ||
	  Tcl_GetIntFromObj( interp, objv [7], & nl ) != TCL_OK ||
	  Tcl_GetIntFromObj( interp, objv [8], & ns ) != TCL_OK))) {
      if (objc == 9)
	Tcl_SetResult( interp, const_cast<char*>( "failed to parse int from image, band, sl, ss, nl, or ns" ), TCL_VOLATILE );
      else
	Tcl_SetResult( interp, const_cast<char*>( "failed to parse int from image or band" ), TCL_VOLATILE );

      free( inPath );
      free( outPath );
      return TCL_ERROR;
    }
  } else {
    image = 0;
    band = 0;
  }

  // get a handle on the input image file
  ossimImageHandler * ih = ossimImageHandlerRegistry::instance()->open( ossimFilename( inPath ) );
  //ossimRefPtr<ossimImageHandler> ih = ossimImageHandlerRegistry::instance()->open( ossimFilename( inPath ) );

  vector< ossim_uint32 > entryList;
  ih->getEntryList( entryList );

  // select the particular image we want
  ih->setCurrentEntry( entryList[ image ] );
  vector< ossim_uint32 > bl;
  ossimRefPtr<ossimBandSelector> bs = new ossimBandSelector();
  bs->connectMyInputTo( ih );
  //ossimRefPtr<ossimImageSourceSequencer> issTmp = new ossimImageSourceSequencer( ih );

  // select the particular band we want
  bl.push_back( band );
  bs->setOutputBandList( bl );
  bs->initialize();
  ossimRefPtr<ossimImageSourceSequencer> iss = new ossimImageSourceSequencer( bs.get() );

  // determine pixel type, image shape, and origin
  ossimScalarType scalerType = iss->getOutputScalarType();
  int scalarSize = ossimGetScalarSizeInBytes( scalerType );
  int tileHeight = iss->getTileHeight();
  ossimIrect irect = iss->getBoundingRect();
  int originX = irect.ul().x;
  int originY = irect.ul().y;
  int sourceNs = irect.ur().x - irect.ul().x + 1;
  int sourceNl = irect.ll().y - irect.ul().y + 1;

  sprintf( sourceNlStr, "%d", sourceNl );
  sprintf( sourceNsStr, "%d", sourceNs );

  // if nl defaulted to 0, then we want the whole image, not just a subarea
  if ( nl == 0 ) {
    nl = sourceNl;
    ns = sourceNs;
  }

  // get geolocation information
  double ul_lat = 0.0, ul_lon = 0.0, ll_lat = 0.0, ur_lon = 0.0;

  char igeolo[ 70 ];
  igeolo[ 0 ] = '\0';

  // if the sides are not parallel in lat/lon, we need to generate a transformation matrix for geotiff
  int isRot = 0;
  char rotBuf[1000];
  rotBuf[0] = '\0';

  {
    int NL, NS;
    double n, e, w, s;

    if (objc == 15 &&
	 (Tcl_GetIntFromObj( interp, objv [9], & NL ) == TCL_OK ||
	  Tcl_GetIntFromObj( interp, objv [10], & NS ) == TCL_OK ||
	  Tcl_GetDoubleFromObj( interp, objv [11], & n ) == TCL_OK ||
	  Tcl_GetDoubleFromObj( interp, objv [12], & e ) == TCL_OK ||
	  Tcl_GetDoubleFromObj( interp, objv [13], & w ) == TCL_OK ||
	  Tcl_GetDoubleFromObj( interp, objv [14], & s ) == TCL_OK)) {

      if ( NL != sourceNl || NS != sourceNs ) {
	Tcl_SetResult( interp, const_cast<char*>( "NL != sourceNl || NS != sourceNs" ), TCL_VOLATILE );
	free( inPath );
	free( outPath );
	return TCL_ERROR;
      }

      ul_lat = n;
      ul_lon = w;
      ll_lat = s;
      ur_lon = e;

    } else
      NL = 0;

    if ( NL > 0 ) {
      // must be a cib
      printf( "cib NL %d NS %d n %lf e %lf w %lf s %lf\n", NL, NS, n, e, w, s );

    } else { // not a cib, use igeolo instead
      printf( "not a cib, getting igeolo\n" );

      char ul_lat_buf[ 8 ];
      char ul_lon_buf[ 9 ];

      char ll_lat_buf[ 8 ];
      char ll_lon_buf[ 9 ];

      char ur_lat_buf[ 8 ];
      char ur_lon_buf[ 9 ];

      ossimRefPtr<ossimNitfFile> nitfFile = new ossimNitfFile();
      nitfFile->parseFile( ossimFilename( inPath ) );
      ossimRefPtr<ossimNitfImageHeader>  hdr = nitfFile->getNewImageHeader( image );
      ossimNitfImageHeaderV2_X* header = PTR_CAST(ossimNitfImageHeaderV2_X, hdr.get());
      strcpy( igeolo, header->getGeographicLocation() );

      printf( "igeolo %s\n", igeolo );

      // ddmmssYdddmmssXddmmssYdddmmssXddmmssYdddmmssXddmmssYdddmmssX
      // 012345678901234567890123456789012345678901234567890123456789
      // ul at 0, 7
      // ur at 15, 22
      // lr at 30, 37
      // ll at 45, 52

      strncpy( ul_lat_buf, igeolo, 7 );
      ul_lat_buf[ 7 ] = 0;

      strncpy( ul_lon_buf, igeolo + 7, 8 );
      ul_lon_buf[ 8 ] = 0;

      strncpy( ur_lat_buf, igeolo + 15, 7 );
      ll_lat_buf[ 7 ] = 0;

      strncpy( ur_lon_buf, igeolo + 22, 8 );
      ur_lon_buf[ 8 ] = 0;

      strncpy( ll_lat_buf, igeolo + 45, 7 );
      ll_lat_buf[ 7 ] = 0;

      strncpy( ll_lon_buf, igeolo + 52, 8 );
      ul_lon_buf[ 8 ] = 0;

      int deg, min, sec;

      // ul lat
      if ( sscanf( ul_lat_buf, "%2d%2d%2d", &deg, &min, &sec ) != 3 )
	printf( "Failed parsing dms from igeolo\n" );

      double igl_ul_lat = deg + min/60.0 + sec/3600.0;

      if ( ul_lat_buf[ 6 ] == 'S' )
	igl_ul_lat *= -1.0;

      // ul lon
      if ( sscanf( ul_lon_buf, "%3d%2d%2d", &deg, &min, &sec ) != 3 )
	printf( "Failed parsing dms from igeolo\n" );

      double igl_ul_lon = deg + min/60.0 + sec/3600.0;

      if ( ul_lon_buf[ 7 ] == 'W' )
	igl_ul_lon *= -1.0;

      // ur lat
      if ( sscanf( ur_lat_buf, "%2d%2d%2d", &deg, &min, &sec ) != 3 )
	printf( "Failed parsing dms from igeolo\n" );

      double igl_ur_lat = deg + min/60.0 + sec/3600.0;

      if ( ur_lat_buf[ 6 ] == 'S' )
	igl_ur_lat *= -1.0;

      // ur lon
      if ( sscanf( ur_lon_buf, "%3d%2d%2d", &deg, &min, &sec ) != 3 )
	printf( "Failed parsing dms from igeolo\n" );

      double igl_ur_lon = deg + min/60.0 + sec/3600.0;

      if ( ur_lon_buf[ 7 ] == 'W' )
	igl_ur_lon *= -1.0;

      // ll lat
      if ( sscanf( ll_lat_buf, "%2d%2d%2d", &deg, &min, &sec ) != 3 )
	printf( "Failed parsing dms from igeolo\n" );

      double igl_ll_lat = deg + min/60.0 + sec/3600.0;

      if ( ll_lat_buf[ 6 ] == 'S' )
	igl_ll_lat *= -1.0;

      // ll lon
      if ( sscanf( ll_lon_buf, "%3d%2d%2d", &deg, &min, &sec ) != 3 )
	printf( "Failed parsing dms from igeolo\n" );

      double igl_ll_lon = deg + min/60.0 + sec/3600.0;

      if ( ll_lon_buf[ 7 ] == 'W' )
	igl_ll_lon *= -1.0;

      // check if rotated from ul, ll, ur
      int lines[] = { 1, sourceNl, 1 };
      int samps[] = { 1, 1, sourceNs };
      double lats[] = { igl_ul_lat, igl_ll_lat, igl_ur_lat };
      double lons[] = { igl_ul_lon, igl_ll_lon, igl_ur_lon };
      isRot = isRotated( lines, samps, lats, lons, rotBuf );

      ul_lat = igl_ul_lat;
      ul_lon = igl_ul_lon;
      ll_lat = igl_ll_lat;
      ur_lon = igl_ur_lon;

      nitfFile = 0;
    } // not a cib
  }

  // we don't use this scale and tiepoint if it's rotated
  printf( "ullat %lf ullon %lf lllat %lf urlon %lf\n", ul_lat, ul_lon, ll_lat, ur_lon );
  double imageLonWidth = ur_lon - ul_lon;
  double imageLatHeight = ul_lat - ll_lat;
  double xScale = imageLonWidth / sourceNs;
  double yScale = imageLatHeight / sourceNl;
  // tiepoint
  double ulLon = ul_lon + imageLonWidth * ((double) ss - 1) / sourceNs;
  double ulLat = ul_lat - imageLatHeight * ((double) sl - 1) / sourceNl;

  int vunit;
  if (zvunit( & vunit, const_cast<char*>( "U_NAME" ), 1, const_cast<char*>( "U_NAME" ), outPath, NULL ) != 1) {
    Tcl_SetResult( interp, const_cast<char*>( "zvunit failed on outPath" ), TCL_VOLATILE );
    free( inPath );
    free( outPath );
    return TCL_ERROR;
  }

  switch ( scalarSize ) {
  case 1:
    if (zvopen( vunit, "U_NL", nl, "U_NS", ns, "OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA", "O_FORMAT", "BYTE", NULL ) != 1) {
      Tcl_SetResult( interp, const_cast<char*>( "zvopen failed on outPath" ), TCL_VOLATILE );
      free( inPath );
      free( outPath );
      return TCL_ERROR;
    }
    break;
  case 2:			// assume half word
    if (zvopen( vunit, "U_NL", nl, "U_NS", ns, "OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA", "O_FORMAT", "HALF", NULL ) != 1) {
      Tcl_SetResult( interp, const_cast<char*>( "zvopen failed on outPath" ), TCL_VOLATILE );
      free( inPath );
      free( outPath );
      return TCL_ERROR;
    }
    break;
  case 4:			// assume real*8
    if (zvopen( vunit, "U_NL", nl, "U_NS", ns, "OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA", "O_FORMAT", "REAL", NULL ) != 1) {
      Tcl_SetResult( interp, const_cast<char*>( "zvopen failed on outPath" ), TCL_VOLATILE );
      free( inPath );
      free( outPath );
      return TCL_ERROR;
    }
    break;
  default:
    Tcl_SetResult( interp, const_cast<char*>( "Unsupported pixel size" ), TCL_VOLATILE );
    free( inPath );
    free( outPath );
    return TCL_ERROR;
  }

  /*
    Add GeoTIFF tags
    should look like this
    MODELTIEPOINTTAG='(0,0,0,-120,32.09,0)'
    MODELPIXELSCALETAG='(0.000250012500625031,0.000327011118378027,0)'
    GTMODELTYPEGEOKEY='2(ModelTypeGeographic);gk_type=Short;gk_key=1024'
    GTRASTERTYPEGEOKEY='1(RasterPixelIsArea);gk_type=Short;gk_key=1025'
    GTCITATIONGEOKEY='MUSE Raster File;gk_key=1026'
    GEOGRAPHICTYPEGEOKEY='4326(GCS_WGS_84);gk_type=Short;gk_key=2048'
    GEOGLINEARUNITSGEOKEY='9001(Linear_Meter);gk_type=Short;gk_key=2052'
    GEOGANGULARUNITSGEOKEY='9102(Angular_Degree);gk_type=Short;gk_key=2054'

    but make it look like this for compatibility, though it's off by half a pixel
    MODELTIEPOINTTAG='(0.0,0.0,0,3.000000000000E+01,1.000000000000E+01,0)'
    MODELPIXELSCALETAG='(0.00048828125,0.00048828125,0.0)'
    GTRASTERTYPEGEOKEY='2(RASTERPIXELISPOINT)'
    GTMODELTYPEGEOKEY='2(MODELTYPEGEOGRAPHIC)'
    GEOGELLIPSOIDGEOKEY='7030(ELLIPSE_WGS84)'

  */

  sprintf (tiePoint, "(0,0,0,%.15lf,%.15lf,0)", ulLon, ulLat);
  sprintf (scale, "(%.16lf,%.15lf,0)", xScale, yScale);

  /* static int isRotated( int* lines, int* samps, double* lats, double* lons, char* labelstr )*/
  if (addStringToProperty ("NITF_IGEOLO", igeolo, 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("NITF_NROWS", sourceNlStr, 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("NITF_NCOLS", sourceNsStr, 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GTMODELTYPEGEOKEY", "2(ModelTypeGeographic);gk_type=Short;gk_key=1024", 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GTRASTERTYPEGEOKEY", "1(RasterPixelIsArea);gk_type=Short;gk_key=1025", 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GTCITATIONGEOKEY", "Extracted by ossimTcl;gk_key=1026", 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GEOGRAPHICTYPEGEOKEY", "4326(GCS_WGS_84);gk_type=Short;gk_key=2048", 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GEOGELLIPSOIDGEOKEY", "7030(ELLIPSE_WGS84);gk_type=Short;gk_key=2056", 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GEOGLINEARUNITSGEOKEY", "9001(Linear_Meter);gk_type=Short;gk_key=2052", 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GEOGANGULARUNITSGEOKEY", "9102(Angular_Degree);gk_type=Short;gk_key=2054", 1, & vunit, "GEOTIFF", 0)) {
    Tcl_SetResult( interp, const_cast<char*>( "addStringToProperty failed on outPath" ), TCL_VOLATILE );
    free( inPath );
    free( outPath );
    return TCL_ERROR;
  }

  if (isRot) {
    if (addStringToProperty ("MODELTRANSFORMATIONTAG", rotBuf, 1, & vunit, "GEOTIFF", 0)) {
      Tcl_SetResult( interp, const_cast<char*>( "addStringToProperty failed on outPath" ), TCL_VOLATILE );
      free( inPath );
      free( outPath );
      return TCL_ERROR;
    }
  } else {
    if (addStringToProperty ("MODELTIEPOINTTAG", tiePoint, 1, & vunit, "GEOTIFF", 0) ||
	addStringToProperty ("MODELPIXELSCALETAG", scale, 1, & vunit, "GEOTIFF", 0)) {
      Tcl_SetResult( interp, const_cast<char*>( "addStringToProperty failed on outPath" ), TCL_VOLATILE );
      free( inPath );
      free( outPath );
      return TCL_ERROR;
    }
  }

  // if inPath ends with ".TOC", then this is RPF (likely CIB) and we don't want tags
  int pathLen = strlen( inPath );
  
  if ( pathLen < 4 ||
       (inPath[ pathLen - 4 ] != '.') ||
       ((inPath[ pathLen - 3 ] | ' ') != 't') ||
       ((inPath[ pathLen - 2 ] | ' ') != 'o') ||
       ((inPath[ pathLen - 1 ] | ' ') != 'c') ) {

    if ( getTags( vunit, inPath, image ) ) {
      Tcl_SetResult( interp, const_cast<char*>( "GetTags failed" ), TCL_VOLATILE );
      free( inPath );
      free( outPath );
      return TCL_ERROR;
    } 
  }

  oneStrip = 0;
  stripLoaded = -1;

  char * imageBuf = 0;
  char * writeBuf = 0;

  for (line = sl; line < sl + nl; line ++) {
    strip = (originY + line - 1) / tileHeight;
    if (strip != stripLoaded) {
      if (oneStrip)
	delete oneStrip;

      // ossimIrect (ul_corner_x, ul_corner_y, lr_corner_x, lr_corner_y)
      lry = ((strip + 1) * tileHeight) - 1;
      if ( lry > sourceNl - 1 )	// possible if last row of tiles not filled
	lry = sourceNl - 1;
      oneStrip = new ossimIrect( originX, strip * tileHeight, originX + sourceNs - 1, lry );

      imageData = iss->getTile( * oneStrip );

      if (imageData != 0) {
	imageBuf = (char *) imageData->getBuf();
      } else {
	imageBuf = 0;
      }

      stripLoaded = strip;
    }

    if (((line - sl) % (nl / 10) == 0))
      cout << "Writing line " << line - sl << "/" << nl << endl;

    /* beginning of source image subarea line */
    char * p = imageBuf + scalarSize * (sourceNs * ((line - 1) % tileHeight) + ss - 1);

    writeBuf = p;

    if (zvwrit (vunit, writeBuf, "LINE", line - sl + 1, "SAMP", 1, "NSAMPS", ns, NULL) != 1) {
      printf ("zvwrit failed\n");
      Tcl_SetResult( interp, const_cast<char*>( "zvwrit failed on outPath" ), TCL_VOLATILE );
      free( inPath );
      free( outPath );
      return TCL_ERROR;
    }
  }

  zvclose (vunit, NULL);

  iss = 0;
  bs = 0;

  return TCL_OK;
}

static int ossimCopyNitfTagsProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  /* sl, ss are one-based */
  const char * usage = "wrong # args: should be \"ossim::copyNitfTags pathToImgOrA.Toc inout.img [image band]\"";

  char tiePoint[100];
  char scale[100];
  char sourceNlStr[20], sourceNsStr[20];
  ossimRefPtr< ossimImageData > imageData;

  // process parameters (see usage string above)
  if (objc != 5 && objc != 3) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * inPath = strdup( Tcl_GetStringFromObj( objv [1], 0 ) );

  if ( ! verifyFile( inPath ) ) {
    Tcl_SetResult( interp, const_cast<char*>( "error opening input file" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * outPath = strdup( Tcl_GetStringFromObj( objv [2], 0 ) );
  int image, band, sl=1, ss=1, nl=0, ns=0;
  
  if (objc > 3 ) {
    if (Tcl_GetIntFromObj( interp, objv [3], & image ) != TCL_OK ||
	Tcl_GetIntFromObj( interp, objv [4], & band ) != TCL_OK) {
      Tcl_SetResult( interp, const_cast<char*>( "failed to parse int from image or band" ), TCL_VOLATILE );

      free( inPath );
      free( outPath );
      return TCL_ERROR;
    }
  } else {
    image = 0;
    band = 0;
  }

  // get a handle on the input image file
  ossimImageHandler * ih = ossimImageHandlerRegistry::instance()->open( ossimFilename( inPath ) );
  //ossimRefPtr<ossimImageHandler> ih = ossimImageHandlerRegistry::instance()->open( ossimFilename( inPath ) );

  vector< ossim_uint32 > entryList;
  ih->getEntryList( entryList );

  // select the particular image we want
  ih->setCurrentEntry( entryList[ image ] );
  vector< ossim_uint32 > bl;
  ossimRefPtr<ossimBandSelector> bs = new ossimBandSelector();
  bs->connectMyInputTo( ih );
  //ossimImageSourceSequencer issTmp( ih );

  // select the particular band we want
  bl.push_back( band );
  bs->setOutputBandList( bl );
  bs->initialize();
  ossimRefPtr<ossimImageSourceSequencer> iss = new ossimImageSourceSequencer( bs.get() );

  // determine pixel type, image shape, and origin
  ossimIrect irect = iss->getBoundingRect();
  int sourceNs = irect.ur().x - irect.ul().x + 1;
  int sourceNl = irect.ll().y - irect.ul().y + 1;

  iss = 0;

  sprintf( sourceNlStr, "%d", sourceNl );
  sprintf( sourceNsStr, "%d", sourceNs );

  // if nl defaulted to 0, then we want the whole image, not just a subarea
  if ( nl == 0 ) {
    nl = sourceNl;
    ns = sourceNs;
  }
  
  // get geolocation information
  double ul_lat = 0.0, ul_lon = 0.0, ll_lat = 0.0, ur_lon = 0.0;

  char igeolo[ 70 ];
  igeolo[ 0 ] = '\0';

  // if the sides are not parallel in lat/lon, we need to generate a transformation matrix for geotiff
  int isRot = 0;
  char rotBuf[1000];
  rotBuf[0] = '\0';

  {
    // this works for cib, but doesn't give us correct results for other images
    ossimKeywordlist keywordList;
    ossimImageGeometry * oig = ih->getImageGeometry();
    if ( ! oig || ! oig->saveState( keywordList ) ) {
      Tcl_SetObjResult( interp, Tcl_NewStringObj( "Failed to get image geometry", -1 ) );
      free( inPath );
      free( outPath );
      return TCL_ERROR;
    }

    const char * s_ul_lat = keywordList.find( "ul_lat" );
    const char * s_ul_lon = keywordList.find( "ul_lon" );
    const char * s_ll_lat = keywordList.find( "ll_lat" );
    const char * s_ur_lon = keywordList.find( "ur_lon" );

    if ( s_ul_lat && s_ul_lon && s_ll_lat && s_ur_lon && // no null values
	 strcmp( s_ul_lat, s_ll_lat ) && // corner lats different
	 strcmp( s_ul_lon, s_ur_lon ) && // corner lons different
	 sscanf( s_ul_lat, "%lf", & ul_lat ) == 1 && // parsed ok
	 sscanf( s_ul_lon, "%lf", & ul_lon ) == 1 && // parsed ok
	 sscanf( s_ll_lat, "%lf", & ll_lat ) == 1 && // parsed ok
	 sscanf( s_ur_lon, "%lf", & ur_lon ) == 1) { // parsed ok
      // must be a cib
      printf( "cib ullat %s ullon %s lllat %s urlon %s\n", s_ul_lat, s_ul_lon, s_ll_lat, s_ur_lon );
      ; // we have our corners
    } else { // not a cib, use igeolo instead
      printf( "not a cib, getting igeolo\n" );

      char ul_lat_buf[ 8 ];
      char ul_lon_buf[ 9 ];

      char ll_lat_buf[ 8 ];
      char ll_lon_buf[ 9 ];

      char ur_lat_buf[ 8 ];
      char ur_lon_buf[ 9 ];

      ossimRefPtr<ossimNitfFile> nitfFile = new ossimNitfFile();
      nitfFile->parseFile( ossimFilename( inPath ) );
      ossimRefPtr<ossimNitfImageHeader>  hdr = nitfFile->getNewImageHeader( image );
      ossimNitfImageHeaderV2_X* header = PTR_CAST(ossimNitfImageHeaderV2_X, hdr.get());
      strcpy( igeolo, header->getGeographicLocation() );

      printf( "igeolo %s\n", igeolo );

      // ddmmssYdddmmssXddmmssYdddmmssXddmmssYdddmmssXddmmssYdddmmssX
      // 012345678901234567890123456789012345678901234567890123456789
      // ul at 0, 7
      // ur at 15, 22
      // lr at 30, 37
      // ll at 45, 52

      strncpy( ul_lat_buf, igeolo, 7 );
      ul_lat_buf[ 7 ] = 0;

      strncpy( ul_lon_buf, igeolo + 7, 8 );
      ul_lon_buf[ 8 ] = 0;

      strncpy( ur_lat_buf, igeolo + 15, 7 );
      ll_lat_buf[ 7 ] = 0;

      strncpy( ur_lon_buf, igeolo + 22, 8 );
      ur_lon_buf[ 8 ] = 0;

      strncpy( ll_lat_buf, igeolo + 45, 7 );
      ll_lat_buf[ 7 ] = 0;

      strncpy( ll_lon_buf, igeolo + 52, 8 );
      ul_lon_buf[ 8 ] = 0;

      int deg, min, sec;

      // ul lat
      if ( sscanf( ul_lat_buf, "%2d%2d%2d", &deg, &min, &sec ) != 3 )
	printf( "Failed parsing dms from igeolo\n" );

      double igl_ul_lat = deg + min/60.0 + sec/3600.0;

      if ( ul_lat_buf[ 6 ] == 'S' )
	igl_ul_lat *= -1.0;

      // ul lon
      if ( sscanf( ul_lon_buf, "%3d%2d%2d", &deg, &min, &sec ) != 3 )
	printf( "Failed parsing dms from igeolo\n" );

      double igl_ul_lon = deg + min/60.0 + sec/3600.0;

      if ( ul_lon_buf[ 7 ] == 'W' )
	igl_ul_lon *= -1.0;

      // ur lat
      if ( sscanf( ur_lat_buf, "%2d%2d%2d", &deg, &min, &sec ) != 3 )
	printf( "Failed parsing dms from igeolo\n" );

      double igl_ur_lat = deg + min/60.0 + sec/3600.0;

      if ( ur_lat_buf[ 6 ] == 'S' )
	igl_ur_lat *= -1.0;

      // ur lon
      if ( sscanf( ur_lon_buf, "%3d%2d%2d", &deg, &min, &sec ) != 3 )
	printf( "Failed parsing dms from igeolo\n" );

      double igl_ur_lon = deg + min/60.0 + sec/3600.0;

      if ( ur_lon_buf[ 7 ] == 'W' )
	igl_ur_lon *= -1.0;

      // ll lat
      if ( sscanf( ll_lat_buf, "%2d%2d%2d", &deg, &min, &sec ) != 3 )
	printf( "Failed parsing dms from igeolo\n" );

      double igl_ll_lat = deg + min/60.0 + sec/3600.0;

      if ( ll_lat_buf[ 6 ] == 'S' )
	igl_ll_lat *= -1.0;

      // ll lon
      if ( sscanf( ll_lon_buf, "%3d%2d%2d", &deg, &min, &sec ) != 3 )
	printf( "Failed parsing dms from igeolo\n" );

      double igl_ll_lon = deg + min/60.0 + sec/3600.0;

      if ( ll_lon_buf[ 7 ] == 'W' )
	igl_ll_lon *= -1.0;

      // check if rotated from ul, ll, ur
      int lines[] = { 1, sourceNl, 1 };
      int samps[] = { 1, 1, sourceNs };
      double lats[] = { igl_ul_lat, igl_ll_lat, igl_ur_lat };
      double lons[] = { igl_ul_lon, igl_ll_lon, igl_ur_lon };
      isRot = isRotated( lines, samps, lats, lons, rotBuf );

      ul_lat = igl_ul_lat;
      ul_lon = igl_ul_lon;
      ll_lat = igl_ll_lat;
      ur_lon = igl_ur_lon;

      nitfFile = 0;
    } // not a cib
  }

  // we don't use this scale and tiepoint if it's rotated
  printf( "ullat %lf ullon %lf lllat %lf urlon %lf\n", ul_lat, ul_lon, ll_lat, ur_lon );
  double imageLonWidth = ur_lon - ul_lon;
  double imageLatHeight = ul_lat - ll_lat;
  double xScale = imageLonWidth / sourceNs;
  double yScale = imageLatHeight / sourceNl;
  // tiepoint
  double ulLon = ul_lon + imageLonWidth * ((double) ss - 1) / sourceNs;
  double ulLat = ul_lat - imageLatHeight * ((double) sl - 1) / sourceNl;

  int vunit;

  if (zvunit( & vunit, const_cast<char*>( "U_NAME" ), 1, const_cast<char*>( "U_NAME" ), (const char *) outPath, NULL ) != 1) {
    Tcl_SetResult( interp, const_cast<char*>( "zvunit failed on outPath" ), TCL_VOLATILE );
    free( inPath );
    free( outPath );
    return TCL_ERROR;
  }

  if ( zvopen( vunit, "OP", "UPDATE", "OPEN_ACT", "SA", "LAB_ACT", "SA", NULL) != 1) {
    Tcl_SetResult( interp, const_cast<char*>( "zvopen failed on outPath" ), TCL_VOLATILE );
      free( inPath );
      free( outPath );
      return TCL_ERROR;
    }

  /*
    Add GeoTIFF tags
    should look like this
    MODELTIEPOINTTAG='(0,0,0,-120,32.09,0)'
    MODELPIXELSCALETAG='(0.000250012500625031,0.000327011118378027,0)'
    GTMODELTYPEGEOKEY='2(ModelTypeGeographic);gk_type=Short;gk_key=1024'
    GTRASTERTYPEGEOKEY='1(RasterPixelIsArea);gk_type=Short;gk_key=1025'
    GTCITATIONGEOKEY='MUSE Raster File;gk_key=1026'
    GEOGRAPHICTYPEGEOKEY='4326(GCS_WGS_84);gk_type=Short;gk_key=2048'
    GEOGLINEARUNITSGEOKEY='9001(Linear_Meter);gk_type=Short;gk_key=2052'
    GEOGANGULARUNITSGEOKEY='9102(Angular_Degree);gk_type=Short;gk_key=2054'

    but make it look like this for compatibility, though it's off by half a pixel
    MODELTIEPOINTTAG='(0.0,0.0,0,3.000000000000E+01,1.000000000000E+01,0)'
    MODELPIXELSCALETAG='(0.00048828125,0.00048828125,0.0)'
    GTRASTERTYPEGEOKEY='2(RASTERPIXELISPOINT)'
    GTMODELTYPEGEOKEY='2(MODELTYPEGEOGRAPHIC)'
    GEOGELLIPSOIDGEOKEY='7030(ELLIPSE_WGS84)'

  */

  sprintf (tiePoint, "(0,0,0,%.15lf,%.15lf,0)", ulLon, ulLat);
  sprintf (scale, "(%.16lf,%.15lf,0)", xScale, yScale);

  if (addStringToProperty ("NITF_IGEOLO", igeolo, 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("NITF_NROWS", sourceNlStr, 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("NITF_NCOLS", sourceNsStr, 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GTMODELTYPEGEOKEY", "2(ModelTypeGeographic);gk_type=Short;gk_key=1024", 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GTRASTERTYPEGEOKEY", "1(RasterPixelIsArea);gk_type=Short;gk_key=1025", 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GTCITATIONGEOKEY", "Extracted by ossimTcl;gk_key=1026", 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GEOGRAPHICTYPEGEOKEY", "4326(GCS_WGS_84);gk_type=Short;gk_key=2048", 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GEOGELLIPSOIDGEOKEY", "7030(ELLIPSE_WGS84);gk_type=Short;gk_key=2056", 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GEOGLINEARUNITSGEOKEY", "9001(Linear_Meter);gk_type=Short;gk_key=2052", 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GEOGANGULARUNITSGEOKEY", "9102(Angular_Degree);gk_type=Short;gk_key=2054", 1, & vunit, "GEOTIFF", 0)) {
    Tcl_SetResult( interp, const_cast<char*>( "addStringToProperty failed on outPath" ), TCL_VOLATILE );
    free( inPath );
    free( outPath );
    return TCL_ERROR;
  }

  if (isRot) {
    if (addStringToProperty ("MODELTRANSFORMATIONTAG", rotBuf, 1, & vunit, "GEOTIFF", 0)) {
      Tcl_SetResult( interp, const_cast<char*>( "addStringToProperty failed on outPath" ), TCL_VOLATILE );
      free( inPath );
      free( outPath );
      return TCL_ERROR;
    }
  } else {
    if (addStringToProperty ("MODELTIEPOINTTAG", tiePoint, 1, & vunit, "GEOTIFF", 0) ||
	addStringToProperty ("MODELPIXELSCALETAG", scale, 1, & vunit, "GEOTIFF", 0)) {
      Tcl_SetResult( interp, const_cast<char*>( "addStringToProperty failed on outPath" ), TCL_VOLATILE );
      free( inPath );
      free( outPath );
      return TCL_ERROR;
    }
  }

  // if inPath ends with ".TOC", then this is RPF (likely CIB) and we don't want tags
  int pathLen = strlen( inPath );
  
  if ( pathLen < 4 ||
       (inPath[ pathLen - 4 ] != '.') ||
       ((inPath[ pathLen - 3 ] | ' ') != 't') ||
       ((inPath[ pathLen - 2 ] | ' ') != 'o') ||
       ((inPath[ pathLen - 1 ] | ' ') != 'c') ) {

    if ( getTags( vunit, inPath, image ) ) {
      Tcl_SetResult( interp, const_cast<char*>( "GetTags failed" ), TCL_VOLATILE );
      free( inPath );
      free( outPath );
      return TCL_ERROR;
    } 
  }

  bs = 0;

  zvclose (vunit, NULL);

  return TCL_OK;
}

static int ossimGetImageHeaderFieldsProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"ossim::getImageHeaderFields pathToImgOrA.Toc index\"";

  if (objc != 3) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * path = strdup( Tcl_GetStringFromObj( objv [1], 0 ) );

  if ( ! verifyFile( path ) ) {
    Tcl_SetResult( interp, const_cast<char*>( "error opening input file" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  int index;

  if (Tcl_GetIntFromObj( interp, objv[ 2 ], & index) != TCL_OK ) {
    Tcl_SetResult (interp, const_cast<char*>( "failed to parse int from index" ), TCL_VOLATILE);
    free( path );
    return TCL_ERROR;
  }


  ossimRefPtr<ossimNitfFile> nitfFile = new ossimNitfFile();
  nitfFile->parseFile( ossimFilename( path ) );

  ostringstream oss;

  //  ossimNitfImageHeader* header = nitfFile->getNewImageHeader( index );
  ossimRefPtr<ossimNitfImageHeader>  hdr = nitfFile->getNewImageHeader( index );
  ossimNitfImageHeaderV2_X* header = PTR_CAST(ossimNitfImageHeaderV2_X, hdr.get());
  if (header) {
    oss << "IID1 (Image ID1): " << header->getImageId() << endl;
    oss << "IDATIM (Image Date & Time): " << header->getAquisitionDateTime() << endl;
    // TGTID Target ID
    // IID2 Image IID2
    oss << "ISCLAS (Image Security Classification): " << header->getSecurityClassification() << endl;
    // ISCLSY Image Security Classification System
    // ISCODE Image Codewords
    // ISCTLH Image Control and Handling
    // ISREL Image Releasing Instructions
    // ISDCTP Image Declassification Type
    // ISDCDT Image Declassification Date
    // ISDCXM Image Declassification Exemption
    // ISDG Image Downgrade
    // ISDGDT Image Downgrade Date
    // ISCLTX Image Classification Text
    // ISCATP Image Classification Authority Type
    // ISCAUT Image Classification Authority
    // ISCRSN Image Classification Reason
    // ISSRDT Image Security Source Date
    // ISCTLN Image Security Control Number
    // ENCRYP Encryption
    oss << "ISORCE (Image Source): " << header->getImageSource() << endl;
    oss << "NROWS (Number of Rows): " << header->getNumberOfRows() << endl;
    oss << "NCOLS (Number of Columns): " << header->getNumberOfCols() << endl;
    oss << "PVTYPE (Pixel Value Type): " << header->getPixelValueType() << endl;
    oss << "IREP (Image Representation): " << header->getRepresentation() << endl;
    oss << "ICAT (Image Category): " << header->getCategory() << endl;
    oss << "ABPP (Actual Bits Per Pixel Per Band): " << header->getActualBitsPerPixelPerBand() << endl;
    // PJUST Pixel Justification
    oss << "ICORDS (Image Coordinate System): " << header->getCoordinateSystem() << endl;
    oss << "IGEOLO (Image Geographic Location): " << header->getGeographicLocation() << endl;
    // NICOM Number of Image Comments
    // ICOMn Image Comment n
    oss << "IC (Compression Code): " << header->getCompressionCode() << endl;
    // COMRAT Compression Rate Code
    oss << "NBANDS (Number of Bands): " << header->getNumberOfBands() << endl;
    // XBANDS Number of Multi-Spectral Bands
    // IREPBANDnn nnth Band Representation
    // ISUBCATnn nnth Band Subcategory
    // IFCnn nnth Band Image Filter Condition
    // IMFLTnn nnth Band Standard Image Filter Code
    // NLUTSnn nnth Band Number of LUTS
    // NELUTnn nnth Band Number of LUT Entries
    // LUTDnnm nnth Band Data of the mth LUT
    // ISYNC Image Sync code
    oss << "IMODE (Image Mode): " << header->getIMode() << endl;
    oss << "NBPR (Number of Blocks Per Row): " << header->getNumberOfBlocksPerRow() << endl;
    oss << "NBPR (Number of Blocks Per Column): " << header->getNumberOfBlocksPerCol() << endl;
    oss << "NPPBH (Number of Pixels Per Block Horizontal): " << header->getNumberOfPixelsPerBlockHoriz() << endl;
    oss << "NPPBV (Number of Pixels Per Block Vertical): " << header->getNumberOfPixelsPerBlockVert() << endl;
    oss << "NBPP (Number of Bits Per Pixel Per Band): " << header->getBitsPerPixelPerBand() << endl;
    oss << "IDLVL (Display Level): " << header->getDisplayLevel() << endl;
    oss << "IALVL (Attachment Level): " << header->getAttachmentLevel() << endl;
    oss << "ILOC (Image Location): " << header->getDataLocation() << endl;
    // IMAG Image Magnificaiton

    Tcl_SetObjResult( interp, Tcl_NewStringObj( oss.str().c_str(), -1 ) );
    free( path );
    nitfFile = 0;
    return TCL_OK;
  } else {
    Tcl_SetResult( interp, const_cast<char*>( "error extracting image header" ), TCL_VOLATILE );
    free( path );
    nitfFile = 0;
    return TCL_ERROR;
  }
}

static int ossimGetFileHeaderFieldsProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"ossim::getFileHeaderFields pathToImgOrA.Toc\"";

  if (objc != 2) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * path = strdup( Tcl_GetStringFromObj( objv [1], 0 ) );

  if ( ! verifyFile( path ) ) {
    Tcl_SetResult( interp, const_cast<char*>( "error opening input file" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  ossimRefPtr<ossimNitfFile> nitfFile = new ossimNitfFile();
  nitfFile->parseFile( ossimFilename( path ) );

  ostringstream oss;

  const ossimNitfFileHeaderV2_X* header = (ossimNitfFileHeaderV2_X*) nitfFile->getHeader();

  ossimString ostr;

  if (( ostr = header->getVersion() ))
    oss << "FVER (File Version): " << ostr << endl;

  if (( ostr = header->getOriginatingStationId() ))
    oss << "OSTAID (Originating Station ID): " << ostr << endl;

  if (( ostr = header->getDateTime() ))
    oss << "FDT (File Date & Time): " << ostr << endl;

  if (( ostr = header->getTitle() ))
    oss << "FTITLE (File Title): " << ostr << endl;

  if (( ostr = header->getSecurityClassification() ))
    oss << "FSCLAS (File Security Classification): " << ostr << endl;

  if (( ostr = header->getOriginatorsName() ))
    oss << "ONAME (Originator's Name): " << ostr << endl;

  if (( ostr = header->getOriginatorsPhone() ))
    oss << "OPHONE (Originator's Phone Number): " << ostr << endl;

  // oss << "FL (File Length): " << header->getFileSize() << endl; says "-1"
  oss << "HL (NITF File Header Length): " << header->getHeaderSize() << endl;
  oss << "NUMI (Number of Image Segments): " << header->getNumberOfImages() << endl;
  oss << "NUMS (Number of Graphic Segments): " << header->getNumberOfGraphics() << endl;
  oss << "NUMT (Number of Text Segments): " << header->getNumberOfTextSegments() << endl;
  oss << "NUMDES (Number of Data Extension Segments): " << header->getNumberOfDataExtSegments() << endl;

  Tcl_SetObjResult( interp, Tcl_NewStringObj( oss.str().c_str(), -1 ) );
  free( path );
  nitfFile = 0;

  return TCL_OK;
}

static int vicarOpenProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"vicar::open path [mode]\"";
  int tclStatus = TCL_ERROR;

  if (objc < 2 || objc > 3) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * inPath = strdup( Tcl_GetStringFromObj( objv [1], 0 ) );

  if ( ! verifyFile( inPath ) ) {
    Tcl_SetResult( interp, const_cast<char*>( "error opening input file" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * mode = 0;

  if (objc >= 3)
    mode = strdup( Tcl_GetStringFromObj( objv [2], 0 ) );
  else
    mode = strdup( "UPDATE" );

  int vunit;
  static int dummy = 0;

  dummy ++;
  if (zvunit (& vunit, const_cast<char*>( "U_NAME" ), dummy, const_cast<char*>( "U_NAME" ), inPath, NULL) == 1 &&
      zvopen (vunit, "OP", mode, "OPEN_ACT", " ", "IO_ACT", " ", NULL) == 1)
    tclStatus = TCL_OK;

  free( inPath );
  free( mode );

  if ( tclStatus == TCL_OK )
    Tcl_SetObjResult( interp, Tcl_NewIntObj( vunit ) );
  else
    Tcl_SetObjResult( interp, Tcl_NewStringObj( "vicar::open failed to open input", -1 ) );

  return tclStatus;
}

static int vicarCloseProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"vicar::close vunit\"";

  if (objc != 2) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  int vunit;

  if (Tcl_GetIntFromObj( interp, objv[ 1 ], & vunit) != TCL_OK ) {
    Tcl_SetResult (interp, const_cast<char*>( "vicar::close failed to parse int from vunit" ), TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (zvclose (vunit, NULL) == 1) {
    return TCL_OK;
  } else {
    Tcl_SetObjResult( interp, Tcl_NewStringObj( "vicar::close failed to close vunit", -1 ) );
    return TCL_ERROR;
  }
}

static int vicarZladdProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"vicar::zladd vunit name value property\"";
  int tclStatus;

  if (objc != 5) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  int vunit;

  if (Tcl_GetIntFromObj( interp, objv[ 1 ], & vunit) != TCL_OK ) {
    Tcl_SetResult (interp, const_cast<char*>( "vicar::zladd failed to parse int from vunit" ), TCL_VOLATILE);
    return TCL_ERROR;
  }

  char * name = strdup( Tcl_GetStringFromObj( objv [2], 0 ) );
  char * value = strdup( Tcl_GetStringFromObj( objv [3], 0 ) );
  char * property = strdup( Tcl_GetStringFromObj( objv [4], 0 ) );

  if (zladd (vunit, const_cast<char*>( "PROPERTY" ),
	     name, value,
	     const_cast<char*>( "PROPERTY" ), property,
	     const_cast<char*>( "FORMAT" ), const_cast<char*>( "STRING" ), NULL) == 1)
    tclStatus = TCL_OK;
  else {
    Tcl_SetObjResult( interp, Tcl_NewStringObj( "vicar::zladd failed to add property", -1 ) );
    tclStatus = TCL_ERROR;
  }

  free( name );
  free( value );
  free( property );

  return tclStatus;
}

static int shpOpenProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"shp::open path\"";
  int tclStatus = TCL_ERROR;

  if ( objc != 2 ) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * path = strdup( Tcl_GetStringFromObj( objv [1], 0 ) );

  SHPHandle shp = SHPOpen( path, "rb" );

  if ( shp ) {
    long handle = (long) shp;

    Tcl_SetObjResult( interp, Tcl_NewLongObj( handle ) );

    tclStatus = TCL_OK;
  }

  free( path );

  if ( tclStatus != TCL_OK )
    Tcl_SetObjResult( interp, Tcl_NewStringObj( "SHPOpen failed", -1 ) );

  return tclStatus;
}

static int shpCloseProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"shp::close handle\"";

  if ( objc != 2 ) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  SHPHandle shp = (SHPInfo*) NULL;

  if ( Tcl_GetLongFromObj( interp, objv[ 1 ], (long*) &shp ) != TCL_OK ) {
    Tcl_SetResult( interp, const_cast<char*>( "shp::close failed to parse long from handle" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  SHPClose( shp );

  return TCL_OK;
}

static int shpCreateProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"shp::create path type\"";
  int tclStatus = TCL_ERROR;

  if ( objc != 3 ) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * path = strdup( Tcl_GetStringFromObj( objv [1], 0 ) );
  char * type = strdup( Tcl_GetStringFromObj( objv [2], 0 ) );

  int nShapeType = SHPT_NULL;

  if ( !strcmp( type, "SHPT_POINT" ) )
    nShapeType = SHPT_POINT;
  //   else if ( !strcmp( type, "SHPT_ARC" ) )
  //     nShapeType = SHPT_ARC;
  //   else if ( !strcmp( type, "SHPT_POLYGON" ) )
  //     nShapeType = SHPT_POLYGON;
  //   else if ( !strcmp( type, "SHPT_MULTIPOINT" ) )
  //     nShapeType = SHPT_MULTIPOINT;
  else {
    Tcl_SetObjResult( interp, Tcl_NewStringObj( "shp::create: type should be one of {SHPT_POINT}", -1 ) );
    return TCL_ERROR;
  }

  if ( nShapeType != SHPT_NULL ) {
    SHPHandle shp = SHPCreate( path, nShapeType );

    if ( shp ) {
      long handle = (long) shp;

      Tcl_SetObjResult( interp, Tcl_NewLongObj( handle ) );
      
      tclStatus = TCL_OK;
    }
  }

  free( path );
  free( type );

  if ( tclStatus != TCL_OK )
    Tcl_SetObjResult( interp, Tcl_NewStringObj( "SHPCreate failed", -1 ) );

  return tclStatus;
}

static int shpGetInfoProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"shp::getInfo handle\"";

  if ( objc != 2 ) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  SHPHandle shp = (SHPInfo*) NULL;

  if ( Tcl_GetLongFromObj( interp, objv[ 1 ], (long*) &shp ) != TCL_OK ) {
    Tcl_SetResult( interp, const_cast<char*>( "shp::getInfo failed to parse long from handle" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  int pnEntities = 0;
  int pnShapeType = 0;
  double padfMinBound[ 4 ];
  double padfMaxBound[ 4 ];

  SHPGetInfo( shp, &pnEntities, &pnShapeType, padfMinBound, padfMaxBound );

  Tcl_Obj* list[4];
  list[0] = Tcl_NewIntObj( pnEntities );

  switch (pnShapeType) {
  case SHPT_POINT:
    list[1] = Tcl_NewStringObj( "SHPT_POINT", -1 );
    break;
  case SHPT_ARC:
    list[1] = Tcl_NewStringObj( "SHPT_ARC", -1 );
    break;
  case SHPT_POLYGON:
    list[1] = Tcl_NewStringObj( "SHPT_POLYGON", -1 );
    break;
  case SHPT_MULTIPOINT:
    list[1] = Tcl_NewStringObj( "SHPT_MULTIPOINT", -1 );
    break;
  default:
    list[1] = Tcl_NewStringObj( "SHPT_UNKNOWN", -1 );
    break;
  }

  Tcl_Obj* minBoundList[ 4 ];
  Tcl_Obj* maxBoundList[ 4 ];
  for (int i = 0; i < 4; ++i ) {
    minBoundList[ i ] = Tcl_NewDoubleObj( padfMinBound[ i ] );
    maxBoundList[ i ] = Tcl_NewDoubleObj( padfMaxBound[ i ] );
  }

  list[2] = Tcl_NewListObj( 4, minBoundList );
  list[3] = Tcl_NewListObj( 4, maxBoundList );

  Tcl_SetObjResult( interp, Tcl_NewListObj( 4, list) );

  return TCL_OK;
}

static int shpReadProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"shp::read handle index\"";

  if ( objc != 3 ) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  SHPHandle shp = (SHPInfo*) NULL;

  if ( Tcl_GetLongFromObj( interp, objv[ 1 ], (long*) &shp ) != TCL_OK ) {
    Tcl_SetResult( interp, const_cast<char*>( "shp::read failed to parse long from handle" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  int index = 0;

  if ( Tcl_GetIntFromObj( interp, objv[ 2 ], &index ) != TCL_OK ) {
    Tcl_SetResult( interp, const_cast<char*>( "shp::read failed to parse int from index" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  SHPObject* obj = SHPReadObject( shp, index );

  if ( ! obj ) {
    Tcl_SetResult( interp, const_cast<char*>( "shp::read: SHPReadObject failed" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  if ( obj->nSHPType == SHPT_NULL )
    Tcl_SetResult( interp, const_cast<char*>( "" ), TCL_VOLATILE );
  else {
    Tcl_Obj* list[2];
    Tcl_Obj* xList[3];
    Tcl_Obj* yList[3];

    Tcl_Obj** xValues = new Tcl_Obj*[ obj->nVertices ];
    Tcl_Obj** yValues = new Tcl_Obj*[ obj->nVertices ];
    
    for (int i = 0; i < obj->nVertices; ++i ) {
      xValues[ i ] = Tcl_NewDoubleObj( obj->padfX[ i ] );
      yValues[ i ] = Tcl_NewDoubleObj( obj->padfY[ i ] );
    }

    xList[ 0 ] = Tcl_NewListObj( obj->nVertices, xValues );
    xList[ 1 ] = Tcl_NewDoubleObj( obj->dfXMin );
    xList[ 2 ] = Tcl_NewDoubleObj( obj->dfXMax );

    yList[ 0 ] = Tcl_NewListObj( obj->nVertices, yValues );
    yList[ 1 ] = Tcl_NewDoubleObj( obj->dfYMin );
    yList[ 2 ] = Tcl_NewDoubleObj( obj->dfYMax );

    list[ 0 ] = Tcl_NewListObj( 3, xList );
    list[ 1 ] = Tcl_NewListObj( 3, yList );

    Tcl_SetObjResult( interp, Tcl_NewListObj( 2, list) );

    delete[] xValues;
    delete[] yValues;
  }

  SHPDestroyObject( obj );

  return TCL_OK;
}

static int shpWritePointProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"shp::writePoint handle x y\"";

  if ( objc != 4 ) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  SHPHandle shp = (SHPInfo*) NULL;

  if ( Tcl_GetLongFromObj( interp, objv[ 1 ], (long*) &shp ) != TCL_OK ) {
    Tcl_SetResult( interp, const_cast<char*>( "shp::writePoint failed to parse long from handle" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  double x, y;

  if ( Tcl_GetDoubleFromObj( interp, objv[ 2 ], &x) != TCL_OK ) {
    Tcl_SetResult( interp, const_cast<char*>( "shp::writePoint failed to parse double from x" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  if ( Tcl_GetDoubleFromObj( interp, objv[ 3 ], &y) != TCL_OK ) {
    Tcl_SetResult( interp, const_cast<char*>( "shp::writePoint failed to parse double from y" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  int nSHPType = SHPT_POINT;
  int nVertices = 1;

  SHPObject *obj = SHPCreateSimpleObject( nSHPType, nVertices, &x, &y, (double*) NULL );
  SHPWriteObject( shp, -1, obj );
  SHPDestroyObject( obj );

  Tcl_SetObjResult( interp, objv[ 2 ] );
  return TCL_OK;
}

static int dbfOpenProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"dbf::open path\"";
  int tclStatus = TCL_ERROR;

  if ( objc != 2 ) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * path = strdup( Tcl_GetStringFromObj( objv [1], 0 ) );

  DBFHandle dbf = DBFOpen( path, "rb" );

  if ( dbf ) {
    long handle = (long) dbf;

    Tcl_SetObjResult( interp, Tcl_NewLongObj( handle ) );

    tclStatus = TCL_OK;
  }

  free( path );

  if ( tclStatus != TCL_OK )
    Tcl_SetObjResult( interp, Tcl_NewStringObj( "DBFOpen failed", -1 ) );

  return tclStatus;
}

static int dbfCreateProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"dbf::create path\"";
  int tclStatus = TCL_ERROR;

  if ( objc != 2 ) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * path = strdup( Tcl_GetStringFromObj( objv [1], 0 ) );

  DBFHandle dbf = DBFCreate( path );

  if ( dbf ) {
    long handle = (long) dbf;

    Tcl_SetObjResult( interp, Tcl_NewLongObj( handle ) );
      
    tclStatus = TCL_OK;
  }

  free( path );

  if ( tclStatus != TCL_OK )
    Tcl_SetObjResult( interp, Tcl_NewStringObj( "DBFCreate failed", -1 ) );

  return tclStatus;
}

static int dbfGetFieldCountProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"dbf::getFieldCount handle\"";

  if ( objc != 2 ) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  DBFHandle dbf = (DBFInfo*) NULL;

  if ( Tcl_GetLongFromObj( interp, objv[ 1 ], (long*) &dbf ) != TCL_OK ) {
    Tcl_SetResult( interp, const_cast<char*>( "dbf::getGetFieldCount failed to parse long from handle" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  int count = DBFGetFieldCount( dbf );

  Tcl_SetObjResult( interp, Tcl_NewIntObj( count ) );
  
  return TCL_OK;
}

static int dbfGetRecordCountProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"dbf::getRecordCount handle\"";

  if ( objc != 2 ) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  DBFHandle dbf = (DBFInfo*) NULL;

  if ( Tcl_GetLongFromObj( interp, objv[ 1 ], (long*) &dbf ) != TCL_OK ) {
    Tcl_SetResult( interp, const_cast<char*>( "dbf::getGetRecordCount failed to parse long from handle" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  int count = DBFGetRecordCount( dbf );

  Tcl_SetObjResult( interp, Tcl_NewIntObj( count ) );
  
  return TCL_OK;
}

static int dbfGetFieldInfoProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"dbf::getFieldInfo handle index\"";

  if ( objc != 3 ) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  DBFHandle dbf = (DBFInfo*) NULL;

  if ( Tcl_GetLongFromObj( interp, objv[ 1 ], (long*) &dbf ) != TCL_OK ) {
    Tcl_SetResult( interp, const_cast<char*>( "dbf::getGetRecordCount failed to parse long from handle" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  int index = 0;

  if ( Tcl_GetIntFromObj( interp, objv[ 2 ], &index ) != TCL_OK ) {
    Tcl_SetResult( interp, const_cast<char*>( "dbf::getGetFieldInfo failed to parse int from index" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char name[ 12 ];
  int width, decimals;
  DBFFieldType type = DBFGetFieldInfo( dbf, index, name, &width, &decimals );
  
  Tcl_Obj* list[4];

  switch (type) {
  case FTString:
    list[0] = Tcl_NewStringObj( "FTString", -1 );
    break;
  case FTInteger:
    list[0] = Tcl_NewStringObj( "FTInteger", -1 );
    break;
  case FTDouble:
    list[0] = Tcl_NewStringObj( "FTDouble", -1 );
    break;
  case FTLogical:
    list[0] = Tcl_NewStringObj( "FTLogical", -1 );
    break;
  case FTInvalid:
  default:
    list[0] = Tcl_NewStringObj( "FTInvalid", -1 );
    break;
  }

  list[1] = Tcl_NewStringObj( name, -1 );
  list[2] = Tcl_NewIntObj( width );
  list[3] = Tcl_NewIntObj( decimals );

  Tcl_SetObjResult( interp, Tcl_NewListObj( 4, list ) );

  return TCL_OK;
}

static int dbfAddFieldProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"dbf::addField handle name width\"";
  int tclStatus = TCL_OK;

  if ( objc != 4 ) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  DBFHandle dbf = (DBFInfo*) NULL;

  if ( Tcl_GetLongFromObj( interp, objv[ 1 ], (long*) &dbf ) != TCL_OK ) {
    Tcl_SetResult( interp, const_cast<char*>( "dbf::addField failed to parse long from handle" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * name = strdup( Tcl_GetStringFromObj( objv [ 2 ], 0 ) );

  int width = 0;

  if ( Tcl_GetIntFromObj( interp, objv[ 3 ], &width ) != TCL_OK ) {
    Tcl_SetResult( interp, const_cast<char*>( "dbf::addField failed to parse int from width" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  if ( DBFAddField( dbf, name, FTString, width, 0 ) < 0 ) {
    Tcl_SetResult( interp, const_cast<char*>( "dbf::getAddField: DBFAddField failed" ), TCL_VOLATILE );
    tclStatus = TCL_ERROR;
  }

  free( name );

  return tclStatus;
}

static int dbfReadStringProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"dbf::readString handle record field\"";

  if ( objc != 4 ) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  DBFHandle dbf = (DBFInfo*) NULL;

  if ( Tcl_GetLongFromObj( interp, objv[ 1 ], (long*) &dbf ) != TCL_OK ) {
    Tcl_SetResult( interp, const_cast<char*>( "dbf::readString failed to parse long from handle" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  int record = 0;

  if ( Tcl_GetIntFromObj( interp, objv[ 2 ], &record ) != TCL_OK ) {
    Tcl_SetResult( interp, const_cast<char*>( "dbf::readString failed to parse int from record" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  int field = 0;

  if ( Tcl_GetIntFromObj( interp, objv[ 3 ], &field ) != TCL_OK ) {
    Tcl_SetResult( interp, const_cast<char*>( "dbf::readString failed to parse int from field" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, (char*) DBFReadStringAttribute( dbf, record, field ), TCL_VOLATILE );

  return TCL_OK;
}

static int dbfWriteStringProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"dbf::writeString handle record field value\"";

  if ( objc != 5 ) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  DBFHandle dbf = (DBFInfo*) NULL;

  if ( Tcl_GetLongFromObj( interp, objv[ 1 ], (long*) &dbf ) != TCL_OK ) {
    Tcl_SetResult( interp, const_cast<char*>( "dbf::writeString failed to parse long from handle" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  int record = 0;

  if ( Tcl_GetIntFromObj( interp, objv[ 2 ], &record ) != TCL_OK ) {
    Tcl_SetResult( interp, const_cast<char*>( "dbf::writeString failed to parse int from record" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  int field = 0;

  if ( Tcl_GetIntFromObj( interp, objv[ 3 ], &field ) != TCL_OK ) {
    Tcl_SetResult( interp, const_cast<char*>( "dbf::writeString failed to parse int from field" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * value = strdup( Tcl_GetStringFromObj( objv [ 4 ], 0 ) );

  DBFWriteStringAttribute( dbf, record, field, value );

  free( value );

  return TCL_OK;
}

static int dbfCloseProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"dbf::close handle\"";

  if ( objc != 2 ) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  DBFHandle dbf = (DBFInfo*) NULL;

  if ( Tcl_GetLongFromObj( interp, objv[ 1 ], (long*) &dbf ) != TCL_OK ) {
    Tcl_SetResult( interp, const_cast<char*>( "dbf::close failed to parse long from handle" ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  DBFClose( dbf );

  return TCL_OK;
}

extern "C" int Ossimtcl_Init( Tcl_Interp *interp ) {
  Tcl_CreateObjCommand( interp, "ossim::getEntryListSize", ossimGetEntryListSizeProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "ossim::getEntryGeometry", ossimGetEntryGeometryProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "ossim::toVicar", ossimToVicarProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "ossim::copyNitfTags", ossimCopyNitfTagsProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "ossim::rawToNitf", ossimRawToNitfProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "ossim::vicarToNitfInt", ossimVicarToNitfIntProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "ossim::vicarToNitfSi", ossimVicarToNitfSiProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "ossim::getNumberOfOutputBands", ossimGetNumberOfOutputBandsProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "ossim::getTagValue", ossimGetTagValueProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "ossim::printTag", ossimPrintTagProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "ossim::getImageHeaderFields", ossimGetImageHeaderFieldsProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "ossim::getFileHeaderFields", ossimGetFileHeaderFieldsProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "vicar::open", vicarOpenProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "vicar::close", vicarCloseProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "vicar::zladd", vicarZladdProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "vicar::getLabel", vicarGetLabelProc, 0, 0 );

  Tcl_CreateObjCommand( interp, "shp::open", shpOpenProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "shp::getInfo", shpGetInfoProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "shp::read", shpReadProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "shp::close", shpCloseProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "shp::create", shpCreateProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "shp::writePoint", shpWritePointProc, 0, 0 );

  Tcl_CreateObjCommand( interp, "dbf::open", dbfOpenProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "dbf::create", dbfCreateProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "dbf::getFieldCount", dbfGetFieldCountProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "dbf::getRecordCount", dbfGetRecordCountProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "dbf::getFieldInfo", dbfGetFieldInfoProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "dbf::addField", dbfAddFieldProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "dbf::readString", dbfReadStringProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "dbf::writeString", dbfWriteStringProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "dbf::close", dbfCloseProc, 0, 0 );

  return TCL_OK;
}
