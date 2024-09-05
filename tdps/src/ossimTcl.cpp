#include <stdlib.h>
#include <fstream>
#include <sstream>
#include <string>

#include <ossimFilename.h>
#include <ossimImageHandlerFactory.h>
#include <ossimCibCadrgTileSource.h>
#include <ossimImageHandler.h>
#include <ossimBandSelector.h>
#include <ossimImageSourceSequencer.h>
#include <ossimNitfFile.h>
#include <ossimNitfTagInformation.h>
#include <ossimNitfImageHeader.h>
#include <ossimNitfTagFactoryRegistry.h>
#include <ossimNitfWriter.h>
#include <support_data/nitf/ossimNitfRpcATag.h>
#include <support_data/nitf/ossimNitfRpcBTag.h>
#include <support_data/nitf/ossimNitfUse00aTag.h>
#include <support_data/nitf/ossimNitfAmtTag.h>
#include <base/property/ossimStringProperty.h>
#include <base/property/ossimContainerProperty.h>

extern "C" {
#include <tcl.h>
#include "zvproto.h"
}

static int ossimGetEntryListSize( char * path ) {
  ossimImageHandler * ih = ossimImageHandlerFactory::instance()->open( ossimFilename( path ) );

  vector< ossim_uint32 > entryList;
  ih->getEntryList( entryList );

  return entryList.size();
}

static int ossimGetEntryListSizeProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  char * usage = "wrong # args: should be \"ossim::getEntryListSize pathToImgOrA.Toc\"";

  if (objc != 2) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  Tcl_SetObjResult( interp, Tcl_NewIntObj( ossimGetEntryListSize( Tcl_GetStringFromObj( objv [1], 0 ) ) ) );

  return TCL_OK;
}

// ossim::getTagValue /net/lorenz/export/data1/nitf/quickbird/03MAY05072951-P1BS-000000050393_01_P005.NTF 0 RPC00B
static int ossimGetTagValueProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  char * usage = "wrong # args: should be \"ossim::getTagValue pathToImgOrA.Toc index tag\"";

  if (objc != 4) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * path = strdup( Tcl_GetStringFromObj( objv [1], 0 ) );
  char * tagName = strdup( Tcl_GetStringFromObj( objv [3], 0 ) );
  int index;
  

  if ( Tcl_GetIntFromObj( interp, objv[ 2 ], & index) != TCL_OK ) {
    Tcl_SetResult (interp, "failed to parse int from index", TCL_VOLATILE);
    free( path );
    return TCL_ERROR;
  }

  if ( index < 0 || index >= ossimGetEntryListSize( path ) ) {
    Tcl_SetResult (interp, "bad image index, must be in range 0 .. entryListSize - 1", TCL_VOLATILE);
    free( path );
    return TCL_ERROR;
  }

  ossimNitfFile nitfFile;
  nitfFile.parseFile( ossimFilename( path ) );

  int tagNotFound = 0;
  int returnValue = TCL_OK;

  ossimNitfTagInformation tagInfo;
  if ( nitfFile.getNewImageHeader( 0 )->getTag( tagInfo, tagName ) ) {
    ossimRefPtr<ossimNitfRegisteredTag> regTag = ossimNitfTagFactoryRegistry::instance()->create( tagName );

    if ( regTag.valid() ) {
      ifstream inputStream( path );
      inputStream.seekg( tagInfo.getTagDataOffset() );
      regTag->parseStream( inputStream );

      ostringstream oss;
      regTag->writeStream( oss );

      cout << "tag: " << oss.str() << endl;

    } else {
      cout << "invalid" << endl;
      tagNotFound = 1;
    }
  } else {
    cout << "getTag failed" << endl;
    tagNotFound = 1;
  }
 
  if ( tagNotFound ) {
    Tcl_SetResult (interp, "ossim::getTagValue: error: tag not found", TCL_VOLATILE);
    returnValue = TCL_ERROR;
  }  else {
    if ( ! strcasecmp( "RPC00A", tagName ) || ! strcasecmp( "RPC00B", tagName ) ) {
      Tcl_SetResult (interp, tagName, TCL_VOLATILE);
    } else {
      Tcl_SetResult (interp, "ossim::getTagValue: error: unsupported tag", TCL_VOLATILE);
      returnValue = TCL_ERROR;
    }
  }
    
  free( tagName );
  free( path );

  return returnValue;
}

static int ossimGetEntryGeometryProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  char * usage = "wrong # args: should be \"ossim::getEntryGeometry pathToImgOrA.Toc index\"";

  if (objc != 3) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * path = strdup( Tcl_GetStringFromObj( objv [1], 0 ) );
  int index;

  if (Tcl_GetIntFromObj( interp, objv[ 2 ], & index) != TCL_OK ) {
    Tcl_SetResult (interp, "failed to parse int from index", TCL_VOLATILE);
    free( path );
    return TCL_ERROR;
  }

  ossimImageHandler * ih = ossimImageHandlerFactory::instance()->open( ossimFilename( path ) );

  vector< ossim_uint32 > entryList;
  ih->getEntryList( entryList );

  ih->setCurrentEntry( entryList[ index ] );

  ossimKeywordlist keywordList;
  if (ih->getImageGeometry( keywordList ) ) {
    Tcl_SetObjResult( interp, Tcl_NewStringObj( (const char *) (keywordList.toString()), -1 ) );
    free( path );
    return TCL_OK;
  } else {
    Tcl_SetObjResult( interp, Tcl_NewStringObj( "Failed to get image geometry", -1 ) );
    cout << "Failed to get image geometry" << endl;
    free( path );
    return TCL_ERROR;
  }
}

static int ossimGetNumberOfOutputBandsProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  char * usage = "wrong # args: should be \"getNumberOfOutputBands pathToImgOrA.Toc index\"";

  if (objc != 3) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * path = strdup( Tcl_GetStringFromObj( objv [1], 0 ) );
  int index;

  if (Tcl_GetIntFromObj( interp, objv[ 2 ], & index) != TCL_OK ) {
    Tcl_SetResult (interp, "failed to parse int from index", TCL_VOLATILE);
    free( path );
    return TCL_ERROR;
  }

  ossimImageHandler * ih = ossimImageHandlerFactory::instance()->open( ossimFilename( path ) );

  vector< ossim_uint32 > entryList;
  ih->getEntryList( entryList );

  ih->setCurrentEntry( entryList[ index ] );

  ossim_uint32 bandCount = ih->getNumberOfOutputBands();

  Tcl_SetObjResult( interp, Tcl_NewIntObj( bandCount ) );

  free( path );

  return TCL_OK;
}

static int addStringToProperty (char * name, char * value, int unitCount, int * vunits, char * property, int noQuotes) {
  FILE * f;
  char msgBuf [100];
  int vunit;
  
  for (vunit = 0; vunit < unitCount; vunit ++)
    if (zladd (vunits [vunit], "PROPERTY",
	       name, value,
	       "PROPERTY", property,
	       "FORMAT", "STRING", 0) != 1)
      return 1;
  return 0;
}

static int ossimRawToNitfProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  /* sl, ss are one-based */
  char * usage = "wrong # args: should be \"ossim::rawToNitf in.ras out.ntf nl ns\"";

  if (objc != 5) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * inPath = strdup( Tcl_GetStringFromObj( objv [1], 0 ) );
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
    Tcl_SetResult( interp, "ossim::rawToNitf failed to create omd file", TCL_VOLATILE );
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

  ossimImageHandler * ih = ossimImageHandlerFactory::instance()->open( ossimFilename( inPath ) );

  ossimNitfWriter nw( outPath, ih );

  nw.execute();

  char * cmdPath = (char *) malloc( strlen( omdPath ) + strlen( "rm " ) + 1 );
  sprintf( cmdPath, "rm %s", omdPath );
  system( cmdPath );
  free( cmdPath );

  free( inPath );
  free( outPath );
  free( nl );
  free( ns );
  free( omdPath );

  return TCL_OK;
}

static char * getProp( char * pName, char * label, char * buf ) {
  char * p1, * p2;

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

static int ossimVicarToNitfProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  /* sl, ss are one-based */
  char * usage = "wrong # args: should be \"ossim::vicarToNitf in.ras out.ntf\"";

  if (objc != 3) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * inPath = strdup( Tcl_GetStringFromObj( objv [1], 0 ) );
  char * outPath = strdup( Tcl_GetStringFromObj( objv [2], 0 ) );

  // Get label size, nl, ns
  FILE * f = fopen( inPath, "r" );

  if ( ! f ) {
    Tcl_SetResult( interp, "ossim::vicarToNitf failed to open inp VICAR file", TCL_VOLATILE );
    return TCL_ERROR;
  }

  char buf[ 42 ];
  fgets( buf, 9, f );

  if ( strcmp( buf, "LBLSIZE=") ) {
    fclose( f );
    Tcl_SetResult( interp, "ossim::vicarToNitf inp not a VICAR file", TCL_VOLATILE );
    return TCL_ERROR;
  }

  int i = 0;

  while ( i < 42 && fgets( buf + i, 2, f ) && buf[ i ] != ' ' )
    i++;

  int lblsize = 0;

  if ( sscanf( buf, "%d", & lblsize ) != 1 ) {
    fclose( f );
    Tcl_SetResult( interp, "ossim::vicarToNitf error parsing LBLSIZE from inp VICAR file", TCL_VOLATILE );
    return TCL_ERROR;
  }

  if ( fseek( f, 0, SEEK_SET ) ) {
    fclose( f );
    Tcl_SetResult( interp, "ossim::vicarToNitf error resetting fp in inp VICAR file", TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * label = (char *) malloc( lblsize );
  if ( ! label ) {
    fclose( f );
    Tcl_SetResult( interp, "ossim::vicarToNitf error mallocing label buffer for inp VICAR file", TCL_VOLATILE );
    return TCL_ERROR;
  }

  if ( fread( label, lblsize, 1, f ) != 1 ) {
    fclose( f );
    Tcl_SetResult( interp, "ossim::vicarToNitf error reading label from inp VICAR file", TCL_VOLATILE );
    return TCL_ERROR;
  }

  fclose( f );

  char * p;

  p = strstr( label, " INTFMT=" );

  if ( ! p ) {
    Tcl_SetResult( interp, "ossim::vicarToNitf error finding INTFMT in label in inp VICAR file", TCL_VOLATILE );
    return TCL_ERROR;
  }

  p += 8; /* skip " INTFMT=" */
  i = 0;

  while ( i < 42 && ( buf[ i ] = p[ i ] ) != ' ' )
    i ++;

  buf[ i ] = '\0';

  char * intfmt = strdup( buf );

  if ( strcmp( intfmt, "'LOW'" ) && strcmp( intfmt, "'HIGH'" ) ) {
    printf( "INTFMT=%s\n", intfmt );
    Tcl_SetResult( interp, "ossim::vicarToNitf INTFMT in VICAR file label is neither LOW nor HIGH", TCL_VOLATILE );
    return TCL_ERROR;
  }

  p = strstr( label, " NL=" );

  if ( ! p ) {
    Tcl_SetResult( interp, "ossim::vicarToNitf error finding NL in label from inp VICAR file", TCL_VOLATILE );
    return TCL_ERROR;
  }

  p += 4; /* skip " NL=" */
  i = 0;

  while ( i < 42 && ( buf[ i ] = p[ i ] ) != ' ' )
    i ++;

  int nl;

  if ( sscanf( buf, "%d", & nl ) != 1 ) {
    Tcl_SetResult( interp, "ossim::vicarToNitf error parsing NL from inp VICAR file label", TCL_VOLATILE );
    return TCL_ERROR;
  }

  p = strstr( label, " NS=" );

  if ( ! p ) {
    Tcl_SetResult( interp, "ossim::vicarToNitf error finding NS in label from inp VICAR file", TCL_VOLATILE );
    return TCL_ERROR;
  }

  p += 4; /* skip " NS=" */
  i = 0;

  while ( i < 42 && ( buf[ i ] = p[ i ] ) != ' ' )
    i ++;

  int ns;

  if ( sscanf( buf, "%d", & ns ) != 1 ) {
    Tcl_SetResult( interp, "ossim::vicarToNitf error parsing NS from inp VICAR file label", TCL_VOLATILE );
    return TCL_ERROR;
  }

  // create omd file
  char * omdPath = (char *) malloc( strlen( inPath ) + 5 ); // plus .omd\0

  strcpy( omdPath, inPath );
  p = strrchr( omdPath, '.' );
  if ( p )
    * p = '\0';
  strcat( omdPath, ".omd" );

  f = fopen( omdPath, "w" );

  if ( ! f ) {
    fprintf( stderr, "fopen failed to create \"%s\"\n", omdPath );
    Tcl_SetResult( interp, "ossim::vicarToNitf failed to create omd file", TCL_VOLATILE );
    return TCL_ERROR;
  }

  fprintf( f, "filename:         %s\n", inPath );
  fprintf( f, "header_size:      %d\n", lblsize );
  fprintf( f, "image_type:       general_raster_bil\n" );
  fprintf( f, "interleave_type:  bil\n" );
  fprintf( f, "number_bands:     1\n" );
  fprintf( f, "number_lines:     %d\n", nl );
  fprintf( f, "number_samples:   %d\n", ns );
  fprintf( f, "scalar_type:      ushort16\n" );
  // it's either big or little
  if ( ! strcmp( intfmt, "'LOW'" ) ) // if it's little
    fprintf( f, "byte_order:       little_endian\n" );
  else				// else it's big
    fprintf( f, "byte_order:       big_endian\n" );
  fprintf( f, "band1.null_value: 0\n" );
  fprintf( f, "band1.min_value:  -32768\n" );
  fprintf( f, "band1.max_value:  32767\n" );

  fclose( f );

  // create nitf file

  ossimImageHandler * ih = ossimImageHandlerFactory::instance()->open( ossimFilename( inPath ) );

  ossimNitfWriter nw( outPath, ih );

  ossimContainerProperty * fileHeader = new ossimContainerProperty( "file_header" ); // from ossimNitfWriter.cpp:151
  ossimContainerProperty * imageHeader = new ossimContainerProperty( "image_header" ); // from ossimNitfWriter.cpp:162

  //    {
  //      ossimNitfUse00aTag * use00aTag = new ossimNitfUse00aTag;
  //      use00aTag->setAngleToNorth("42");
  //      ossimRefPtr<ossimNitfRegisteredTag> registeredTag = use00aTag;
  //      nw.addRegisteredTag(registeredTag);
  //    }

  {
    char * buf = 0;
    char igeoloBuf[ 61 ];
    char latLonBuf[ 10 ];
    float f;

    igeoloBuf[ 0 ] = '\0';

    if ( ( buf = getProp( "NITF_CORNERLAT1='", label, buf ) ) ) {
      sscanf( buf, "%f", &f );
      sprintf( latLonBuf, "%+07.3f", f );
      strcat( igeoloBuf, latLonBuf );
    }

    if ( ( buf = getProp( "NITF_CORNERLON1='", label, buf ) ) ) {
      sscanf( buf, "%f", &f );
      sprintf( latLonBuf, "%+08.3f", f );
      strcat( igeoloBuf, latLonBuf );
    }

    if ( ( buf = getProp( "NITF_CORNERLAT2='", label, buf ) ) ) {
      sscanf( buf, "%f", &f );
      sprintf( latLonBuf, "%+07.3f", f );
      strcat( igeoloBuf, latLonBuf );
    }

    if ( ( buf = getProp( "NITF_CORNERLON2='", label, buf ) ) ) {
      sscanf( buf, "%f", &f );
      sprintf( latLonBuf, "%+08.3f", f );
      strcat( igeoloBuf, latLonBuf );
    }

    if ( ( buf = getProp( "NITF_CORNERLAT3='", label, buf ) ) ) {
      sscanf( buf, "%f", &f );
      sprintf( latLonBuf, "%+07.3f", f );
      strcat( igeoloBuf, latLonBuf );
    }

    if ( ( buf = getProp( "NITF_CORNERLON3='", label, buf ) ) ) {
      sscanf( buf, "%f", &f );
      sprintf( latLonBuf, "%+08.3f", f );
      strcat( igeoloBuf, latLonBuf );
    }

    if ( ( buf = getProp( "NITF_CORNERLAT4='", label, buf ) ) ) {
      sscanf( buf, "%f", &f );
      sprintf( latLonBuf, "%+07.3f", f );
      strcat( igeoloBuf, latLonBuf );
    }

    if ( ( buf = getProp( "NITF_CORNERLON4='", label, buf ) ) ) {
      sscanf( buf, "%f", &f );
      sprintf( latLonBuf, "%+08.3f", f );
      strcat( igeoloBuf, latLonBuf );
    }

    ossimStringProperty * clevel = new ossimStringProperty( "clevel" ); // from ossimNitfFileHeaderV2_1.cpp:30
    if ( MAX( nl, ns ) > 2047 )
      clevel->setValue( "05" );
    else
      clevel->setValue( "03" );
    fileHeader->addChild( clevel );

    if ( ( buf = getProp( "AMT_LOCATION='", label, buf ) ) ) {
      ossimStringProperty * ostaid = new ossimStringProperty( "ostaid" ); // from ossimNitfFileHeaderV2_1.cpp:32
      ostaid->setValue( buf );
      fileHeader->addChild( ostaid );
    }

    if ( ( buf = getProp( "AMT_FRAME_NUMBER='", label, buf ) ) ) {
      ossimStringProperty * iid1 = new ossimStringProperty( "iid1" ); // from ossimNitfImageHeaderV2_1.cpp:28
      iid1->setValue( buf );
      imageHeader->addChild( iid1 );
    }

    if ( ( buf = getProp( "AMT_FILE_NAME='", label, buf ) ) ) {
      ossimStringProperty * iid2 = new ossimStringProperty( "iid2" ); // from ossimNitfImageHeaderV2_1.cpp:31
      iid2->setValue( buf );
      imageHeader->addChild( iid2 );
    }

    if ( ( buf = getProp( "AMT_COLLECTION_ID='", label, buf ) ) ) {
      ossimStringProperty * ftitle = new ossimStringProperty( "ftitle" ); // from ossimNitfFileHeaderV2_1.cpp:34
      ftitle->setValue( buf );
      fileHeader->addChild( ftitle );
    }

//      if ( ( buf = getProp( "AMT_FILTER_WHEEL_INDEX='", label, buf ) ) ) {
//        ossimStringProperty * isubcat1 = new ossimStringProperty( "isubcat1" ); // from ossimNitfImageHeaderV2_1.cpp:31
//        isubcat1->setValue( buf );
//        ocp->addChild( isubcat1 );
//      }

    ossimStringProperty * fsclas = new ossimStringProperty( "fsclas" ); // from ossimNitfFileHeaderV2_1.cpp:35
    fsclas->setValue( "U" );
    fileHeader->addChild( fsclas );

    ossimStringProperty * fscop = new ossimStringProperty( "fscop" ); // from ossimNitfFileHeaderV2_1.cpp:51
    fscop->setValue( "00000" );
    fileHeader->addChild( fscop );

    ossimStringProperty * fscpys = new ossimStringProperty( "fscpys" ); // from ossimNitfFileHeaderV2_1.cpp:52
    fscpys->setValue( "00000" );
    fileHeader->addChild( fscpys );

    ossimStringProperty * encryp = new ossimStringProperty( "encryp" ); // from ossimNitfFileHeaderV2_1.cpp:53
    encryp->setValue( "0" );
    fileHeader->addChild( encryp );

    ossimStringProperty * isclas = new ossimStringProperty( "isclas" ); // from ossimNitfImageHeaderV2_1.cpp:32
    isclas->setValue( "U" );
    imageHeader->addChild( isclas );

    ossimStringProperty * icat = new ossimStringProperty( "icat" ); // from ossimNitfImageHeaderV2_1.cpp:54
    icat->setValue( "VIS" );
    imageHeader->addChild( icat );

    ossimStringProperty * abpp = new ossimStringProperty( "abpp" ); // from ossimNitfImageHeaderV2_1.cpp:55
    abpp->setValue( "12" );
    imageHeader->addChild( abpp );

    ossimStringProperty * pjust = new ossimStringProperty( "pjust" ); // from ossimNitfImageHeaderV2_1.cpp:56
    pjust->setValue( "R" );
    imageHeader->addChild( pjust );

    ossimStringProperty * icords = new ossimStringProperty( "icords" ); // from ossimNitfImageHeaderV2_1.cpp:57
    icords->setValue( "D" );
    imageHeader->addChild( icords );

    ossimStringProperty * igeolo = new ossimStringProperty( "igeolo" ); // from ossimNitfImageHeaderV2_1.cpp:58
    igeolo->setValue( igeoloBuf );
    imageHeader->addChild( igeolo );

    ossimStringProperty * imode = new ossimStringProperty( "imode" ); // from ossimNitfImageHeaderV2_1.cpp:65
    imode->setValue( "B" );
    imageHeader->addChild( imode );

    ossimStringProperty * nbpp = new ossimStringProperty( "nbpp" ); // from ossimNitfImageHeaderV2_1.cpp:70
    nbpp->setValue( "16" );
    imageHeader->addChild( nbpp );

    if ( ( buf = getProp( "AMT_FILE_TIME='", label, buf ) ) ) {
      ossimStringProperty * idatim = new ossimStringProperty( "idatim" ); // from ossimNitfImageHeaderV2_1.cpp:29
      idatim->setValue( buf );
      imageHeader->addChild( idatim );
    }

    nw.setProperty( fileHeader );
    nw.setProperty( imageHeader );
  }

  {
    ossimRefPtr< ossimNitfRpcBase > rpcTag;
    char * buf = 0;

    if ( ( buf = getProp( "AMT_RPC_TYPE='", label, buf ) )  &&
	 ! strcmp( buf, "RPC00A" ) )
      rpcTag = new ossimNitfRpcATag;
    else
      rpcTag = new ossimNitfRpcBTag;

    if ( ( buf = getProp( "RPC_FIELD1='", label, buf ) ) )
      if ( *buf == '1' )
	rpcTag->setSuccess( true );
      else
	rpcTag->setSuccess( false );
    if ( ( buf = getProp( "RPC_FIELD2='", label, buf ) ) )
      rpcTag->setErrorBias( buf );
    if ( ( buf = getProp( "RPC_FIELD3='", label, buf ) ) )
      rpcTag->setErrorRand( buf );
    if ( ( buf = getProp( "RPC_FIELD4='", label, buf ) ) )
      rpcTag->setLineOffset( buf );
    if ( ( buf = getProp( "RPC_FIELD5='", label, buf ) ) )
      rpcTag->setSampleOffset( buf );
    if ( ( buf = getProp( "RPC_FIELD6='", label, buf ) ) )
      rpcTag->setGeodeticLatOffset( buf );
    if ( ( buf = getProp( "RPC_FIELD7='", label, buf ) ) )
      rpcTag->setGeodeticLonOffset( buf );
    if ( ( buf = getProp( "RPC_FIELD8='", label, buf ) ) )
      rpcTag->setGeodeticHeightOffset( buf );
    if ( ( buf = getProp( "RPC_FIELD9='", label, buf ) ) )
      rpcTag->setLineScale( buf );
    if ( ( buf = getProp( "RPC_FIELD10='", label, buf ) ) )
      rpcTag->setSampleScale( buf );
    if ( ( buf = getProp( "RPC_FIELD11='", label, buf ) ) )
      rpcTag->setGeodeticLatScale( buf );
    if ( ( buf = getProp( "RPC_FIELD12='", label, buf ) ) )
      rpcTag->setGeodeticLonScale( buf );
    if ( ( buf = getProp( "RPC_FIELD13='", label, buf ) ) )
      rpcTag->setGeodeticHeightScale( buf );

    char nameBuf[ 20 ];

    for (int i = 1; i <= 20; i ++) {
      sprintf( nameBuf, "RPC_FIELD14%d='", i );
      if ( ( buf = getProp( nameBuf, label, buf ) ) )
	rpcTag->setLineNumeratorCoeff( i - 1, buf );
    }

    for (int i = 1; i <= 20; i ++) {
      sprintf( nameBuf, "RPC_FIELD15%d='", i );
      if ( ( buf = getProp( nameBuf, label, buf ) ) )
	rpcTag->setLineDenominatorCoeff( i - 1, buf );
    }

    for (int i = 1; i <= 20; i ++) {
      sprintf( nameBuf, "RPC_FIELD16%d='", i );
      if ( ( buf = getProp( nameBuf, label, buf ) ) )
	rpcTag->setSampleNumeratorCoeff( i - 1, buf );
    }

    for (int i = 1; i <= 20; i ++) {
      sprintf( nameBuf, "RPC_FIELD17%d='", i );
      if ( ( buf = getProp( nameBuf, label, buf ) ) )
	rpcTag->setSampleDenominatorCoeff( i - 1, buf );
    }

    ossimRefPtr<ossimNitfRegisteredTag> registeredTag = rpcTag.get();
    nw.addRegisteredTag(registeredTag);
  }

  {
    ossimNitfAmtTag * amtTag = new ossimNitfAmtTag;
    char * buf = 0;

    if ( ( buf = getProp( "AMT_CMD_CAL_BIT='", label, buf ) ) )
      amtTag->setCmdCalBit( buf );
    if ( ( buf = getProp( "AMT_COLLECTION_ID='", label, buf ) ) )
      amtTag->setCollectionId( buf );
    if ( ( buf = getProp( "AMT_COMMAND_ID='", label, buf ) ) )
      amtTag->setCommandId( buf );
    if ( ( buf = getProp( "AMT_COMMAND_NAME='", label, buf ) ) )
      amtTag->setCommandName( buf );
    if ( ( buf = getProp( "AMT_END_LINE='", label, buf ) ) )
      amtTag->setEndLine( buf );
    if ( ( buf = getProp( "AMT_END_SAMPLE='", label, buf ) ) )
      amtTag->setEndSample( buf );
    if ( ( buf = getProp( "AMT_EXECUTABLE_NAME='", label, buf ) ) )
      amtTag->setExecutableName( buf );
    if ( ( buf = getProp( "AMT_FILE_NAME='", label, buf ) ) )
      amtTag->setFileName( buf );
    if ( ( buf = getProp( "AMT_FILE_TIME='", label, buf ) ) )
      amtTag->setFileTime( buf );
    if ( ( buf = getProp( "AMT_FILE_VERSION='", label, buf ) ) )
      amtTag->setFileVersion( buf );
    if ( ( buf = getProp( "AMT_FILTER_NAME='", label, buf ) ) )
      amtTag->setFilterName( buf );
    if ( ( buf = getProp( "AMT_FILTER_WHEEL_INDEX='", label, buf ) ) )
      amtTag->setFilterWheelIndex( buf );
    if ( ( buf = getProp( "AMT_FIRST_FRAME_TIME='", label, buf ) ) )
      amtTag->setFirstFrameTime( buf );
    if ( ( buf = getProp( "AMT_FPA_GAIN_LEVEL='", label, buf ) ) )
      amtTag->setFpaGainLevel( buf );
    if ( ( buf = getProp( "AMT_FPA_GAIN_REGISTER='", label, buf ) ) )
      amtTag->setFpaGainRegister( buf );
    if ( ( buf = getProp( "AMT_FRAME_DELAY='", label, buf ) ) )
      amtTag->setFrameDelay( buf );
    if ( ( buf = getProp( "AMT_FRAME_NUMBER='", label, buf ) ) )
      amtTag->setFrameNumber( buf );
    if ( ( buf = getProp( "AMT_FRAME_PACKET_TIME='", label, buf ) ) )
      amtTag->setFramePacketTime( buf );
    if ( ( buf = getProp( "AMT_INTEGRATION_ROWS='", label, buf ) ) )
      amtTag->setIntegrationRows( buf );
    if ( ( buf = getProp( "AMT_INTEGRATION_TIME='", label, buf ) ) )
      amtTag->setIntegrationTime( buf );
    if ( ( buf = getProp( "AMT_LAST_FRAME_TIME='", label, buf ) ) )
      amtTag->setLastFrameTime( buf );
    if ( ( buf = getProp( "AMT_LINEAR_STAGE_OFFSET='", label, buf ) ) )
      amtTag->setLinearStageOffset( buf );
    if ( ( buf = getProp( "AMT_LINEAR_STAGE_STEPS='", label, buf ) ) )
      amtTag->setLinearStageSteps( buf );
    if ( ( buf = getProp( "AMT_LOCATION='", label, buf ) ) )
      amtTag->setLocation( buf );
    if ( ( buf = getProp( "AMT_NUMBER_LINES='", label, buf ) ) )
      amtTag->setNumberLines( buf );
    if ( ( buf = getProp( "AMT_NUMBER_SAMPLES='", label, buf ) ) )
      amtTag->setNumberSamples( buf );
    if ( ( buf = getProp( "AMT_OPERATING_SYSTEM='", label, buf ) ) )
      amtTag->setOperatingSystem( buf );
    if ( ( buf = getProp( "AMT_PILA='", label, buf ) ) )
      amtTag->setPila( buf );
    if ( ( buf = getProp( "AMT_PIXEL_DATA_BITS='", label, buf ) ) )
      amtTag->setPixelDataBits( buf );
    if ( ( buf = getProp( "AMT_PIXEL_STORAGE_BITS='", label, buf ) ) )
      amtTag->setPixelStorageBits( buf );
    if ( ( buf = getProp( "AMT_PRODUCTION_TIME='", label, buf ) ) )
      amtTag->setProductionTime( buf );
    if ( ( buf = getProp( "AMT_PRODUCT_SUBTYPE='", label, buf ) ) )
      amtTag->setProductSubtype( buf );
    if ( ( buf = getProp( "AMT_PRODUCT_TYPE='", label, buf ) ) )
      amtTag->setProductType( buf );
    if ( ( buf = getProp( "AMT_RPC_TYPE='", label, buf ) ) )
      amtTag->setRpcType( buf );
    if ( ( buf = getProp( "AMT_SOFTWARE_VERSION='", label, buf ) ) )
      amtTag->setSoftwareVersion( buf );
    if ( ( buf = getProp( "AMT_STARTADDR_H='", label, buf ) ) )
      amtTag->setStartaddrH( buf );
    if ( ( buf = getProp( "AMT_STARTADDR_V='", label, buf ) ) )
      amtTag->setStartaddrV( buf );
    if ( ( buf = getProp( "AMT_START_LINE='", label, buf ) ) )
      amtTag->setStartLine( buf );
    if ( ( buf = getProp( "AMT_START_SAMPLE='", label, buf ) ) )
      amtTag->setStartSample( buf );
    if ( ( buf = getProp( "AMT_STOPADDR_H='", label, buf ) ) )
      amtTag->setStopaddrH( buf );
    if ( ( buf = getProp( "AMT_STOPADDR_V='", label, buf ) ) )
      amtTag->setStopaddrV( buf );
    if ( ( buf = getProp( "AMT_TARGET_IDENTIFIER='", label, buf ) ) )
      amtTag->setTargetIdentifier( buf );
    if ( ( buf = getProp( "AMT_TOTAL_FRAMES='", label, buf ) ) )
      amtTag->setTotalFrames( buf );

    ossimRefPtr<ossimNitfRegisteredTag> registeredTag = amtTag;
    nw.addRegisteredTag(registeredTag);
  }

  nw.execute();

  char * cmdPath = (char *) malloc( strlen( omdPath ) + strlen( "rm " ) + 1 );
  sprintf( cmdPath, "rm %s", omdPath );
  system( cmdPath );
  free( cmdPath );

  free( label );
  free( inPath );
  free( outPath );
  free( omdPath );

  return TCL_OK;
}

static int ossimToVicarProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  /* sl, ss are one-based */
  char * usage = "wrong # args: should be \"ossim::toVicar pathToImgOrA.Toc out.img [image band] [sl ss nl ns]\"";

  char tiePoint[100];
  char scale[100];
  int stripLoaded = -1;
  ossimRefPtr< ossimImageData > imageData;
  ossimIrect * oneStrip = 0;
  char * imageBuf;
  int strip;
  int line;
  int lry;

  if (objc != 9 && objc != 5 && objc != 3) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * inPath = strdup( Tcl_GetStringFromObj( objv [1], 0 ) );
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
	Tcl_SetResult( interp, "failed to parse int from image, band, sl, ss, nl, or ns", TCL_VOLATILE );
      else
	Tcl_SetResult( interp, "failed to parse int from image or band", TCL_VOLATILE );

      free( inPath );
      free( outPath );
      return TCL_ERROR;
    }
  } else {
    image = 0;
    band = 0;
  }

  ossimImageHandler * ih = ossimImageHandlerFactory::instance()->open( ossimFilename( inPath ) );

  vector< ossim_uint32 > entryList;
  ih->getEntryList( entryList );

  ih->setCurrentEntry( entryList[ image ] );

  vector< ossim_uint32 > bl;
  ossimBandSelector bs;
  bs.connectMyInputTo( ih );
  ossimImageSourceSequencer issTmp( ih );
  bl.push_back( band );
  bs.setOutputBandList( bl );
  bs.initialize();
  ossimImageSourceSequencer iss( & bs );

  ossimScalarType scalerType = iss.getOutputScalarType();
  int scalarSize = ossimGetScalarSizeInBytes( scalerType );
  int horizTiles = iss.getNumberOfTilesHorizontal();
  int vertTiles = iss.getNumberOfTilesVertical();
  int tileWidth = iss.getTileWidth();
  int tileHeight = iss.getTileHeight();
  ossimIrect irect = iss.getBoundingRect();
  int sourceNs = irect.ur().x - irect.ul().x + 1;
  int sourceNl = irect.ll().y - irect.ul().y + 1;

  if ( nl == 0 ) {
    nl = sourceNl;
    ns = sourceNs;
  }
  
  ossimKeywordlist keywordList;
  if (! ih->getImageGeometry( keywordList ) ) {
    Tcl_SetObjResult( interp, Tcl_NewStringObj( "Failed to get image geometry", -1 ) );
    free( inPath );
    free( outPath );
    return TCL_ERROR;
  }

  const char * ul_lat = keywordList.find( "ul_lat" );
  const char * ul_lon = keywordList.find( "ul_lon" );
  const char * ll_lat = keywordList.find( "ll_lat" );
  const char * ur_lon = keywordList.find( "ur_lon" );
  double d_ul_lat, d_ul_lon, d_ll_lat, d_ur_lon;

  if (sscanf( ul_lat, "%lf", & d_ul_lat ) != 1 ||
      sscanf( ul_lon, "%lf", & d_ul_lon ) != 1 ||
      sscanf( ll_lat, "%lf", & d_ll_lat ) != 1 ||
      sscanf( ur_lon, "%lf", & d_ur_lon ) != 1) {
    Tcl_SetResult( interp, "Failed extracting image corners", TCL_VOLATILE );
    free( inPath );
    free( outPath );
    return TCL_ERROR;
  }

  double imageLonWidth = d_ur_lon - d_ul_lon;
  double imageLatHeight = d_ul_lat - d_ll_lat;
  double xScale = imageLonWidth / sourceNs;
  double yScale = imageLatHeight / sourceNl;
  double ulLon = d_ul_lon + imageLonWidth * ((double) ss - 1) / sourceNs;
  double ulLat = d_ul_lat - imageLatHeight * ((double) sl - 1) / sourceNl;

  int vunit;
  if (zvunit( & vunit, "U_NAME", 1, "U_NAME", (const char *) outPath, 0 ) != 1) {
    Tcl_SetResult( interp, "zvunit failed on outPath", TCL_VOLATILE );
    free( inPath );
    free( outPath );
    return TCL_ERROR;
  }

  switch ( scalarSize ) {
  case 1:
    if (zvopen( vunit, "U_NL", nl, "U_NS", ns, "OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA", "O_FORMAT", "BYTE", 0 ) != 1) {
      Tcl_SetResult( interp, "zvopen failed on outPath", TCL_VOLATILE );
      free( inPath );
      free( outPath );
      return TCL_ERROR;
    }
    break;
  case 2:
    if (zvopen( vunit, "U_NL", nl, "U_NS", ns, "OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA", "O_FORMAT", "HALF", 0 ) != 1) {
      Tcl_SetResult( interp, "zvopen failed on outPath", TCL_VOLATILE );
      free( inPath );
      free( outPath );
      return TCL_ERROR;
    }
    break;
  default:
    Tcl_SetResult( interp, "Failed extracting image corners", TCL_VOLATILE );
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

#ifdef TDPS_GEOTIFF
  if (addStringToProperty ("MODELTIEPOINTTAG", tiePoint, 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("MODELPIXELSCALETAG", scale, 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GTMODELTYPEGEOKEY", "2(ModelTypeGeographic);gk_type=Short;gk_key=1024", 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GTRASTERTYPEGEOKEY", "2(RasterPixelIsPoint);gk_type=Short;gk_key=1025", 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GEOGELLIPSOIDGEOKEY", "7030(Ellipse_WGS_84);gk_type=Short;gk_key=2056", 1, & vunit, "GEOTIFF", 0)) {
      Tcl_SetResult( interp, "addStringToProperty failed on outPath", TCL_VOLATILE );
      free( inPath );
      free( outPath );
      return TCL_ERROR;
  }
#endif
#ifdef AFIDS_GEOTIFF
  if (addStringToProperty ("MODELTIEPOINTTAG", tiePoint, 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("MODELPIXELSCALETAG", scale, 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GTMODELTYPEGEOKEY", "2(ModelTypeGeographic);gk_type=Short;gk_key=1024", 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GTRASTERTYPEGEOKEY", "1(RasterPixelIsArea);gk_type=Short;gk_key=1025", 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GTCITATIONGEOKEY", "Extracted by ossimTcl;gk_key=1026", 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GEOGRAPHICTYPEGEOKEY", "4326(GCS_WGS_84);gk_type=Short;gk_key=2048", 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GEOGLINEARUNITSGEOKEY", "9001(Linear_Meter);gk_type=Short;gk_key=2052", 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GEOGANGULARUNITSGEOKEY", "9102(Angular_Degree);gk_type=Short;gk_key=2054", 1, & vunit, "GEOTIFF", 0)) {
      Tcl_SetResult( interp, "addStringToProperty failed on outPath", TCL_VOLATILE );
      free( inPath );
      free( outPath );
      return TCL_ERROR;
  }
#endif

  oneStrip = 0;
  stripLoaded = -1;

  for (line = sl; line < sl + nl; line ++) {
    strip = (line - 1) / tileHeight;
    if (strip != stripLoaded) {
      if (oneStrip)
	delete oneStrip;

      // ossimIrect (ul_corner_x, ul_corner_y, lr_corner_x, lr_corner_y)
      lry = ((strip + 1) * tileHeight) - 1;
      if ( lry > sourceNl - 1 )	// possible if last row of tiles not filled
	lry = sourceNl - 1;
      oneStrip = new ossimIrect( 0, strip * tileHeight, sourceNs - 1, lry );

      imageData = iss.getTile( * oneStrip );

      if (imageData != NULL) {
	imageBuf = (char *) imageData->getBuf();
      } else {
	imageBuf = NULL;
      }

      stripLoaded = strip;
    }

    if (((line - sl) % (nl / 10) == 0))
      cout << "Writing line " << line - sl << "/" << nl << endl;

    if (zvwrit (vunit, (imageBuf + scalarSize * (sourceNs * ((line - 1) % tileHeight) + ss - 1)), "LINE", line - sl + 1, "SAMP", 1, "NSAMPS", ns, 0) != 1) {
      printf ("zvwrit failed\n");
      Tcl_SetResult( interp, "zvwrit failed on outPath", TCL_VOLATILE );
      free( inPath );
      free( outPath );
      return TCL_ERROR;
    }
  }

  zvclose (vunit, 0);

  return TCL_OK;
}

extern "C" int Ossimtcl_Init( Tcl_Interp *interp ) {
  Tcl_CreateObjCommand( interp, "ossim::getEntryListSize", ossimGetEntryListSizeProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "ossim::getEntryGeometry", ossimGetEntryGeometryProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "ossim::toVicar", ossimToVicarProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "ossim::rawToNitf", ossimRawToNitfProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "ossim::vicarToNitf", ossimVicarToNitfProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "ossim::getNumberOfOutputBands", ossimGetNumberOfOutputBandsProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "ossim::getTagValue", ossimGetTagValueProc, 0, 0 );
  return TCL_OK;
}
