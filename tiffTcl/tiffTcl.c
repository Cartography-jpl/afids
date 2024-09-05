#include <tcl.h>
#include <tiffio.h>
#include <geotiffio.h>
#include <geo_tiffp.h>
#include <geo_keyp.h>

static void
tiffTclWarningHandler(const char* module, const char* fmt, va_list ap)
{
  /* copied from tif_unix.c */
  /*    if (module != NULL) */
  /*      fprintf(stderr, "%s: ", module); */
  /*    fprintf(stderr, "Warning, "); */
  /*    vfprintf(stderr, fmt, ap); */
  /*    fprintf(stderr, ".\n"); */
}

static int tiffTclGetWidthProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  char * usage = "wrong # args: should be \"tiffTcl::getWidth path\"";
  int width;

  if ( objc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  TIFF* in = TIFFOpen( Tcl_GetStringFromObj( objv[ 1 ], 0 ), "r" );
  if ( ! in ) {
    Tcl_SetResult( interp, "error opening tiff image", TCL_VOLATILE );
    return TCL_ERROR;
  }

  TIFFGetField( in, TIFFTAG_IMAGEWIDTH, &width );

  Tcl_SetObjResult( interp, Tcl_NewIntObj( width ) );

  TIFFClose( in );

  return TCL_OK;
}

static int tiffTclGetHeightProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  char * usage = "wrong # args: should be \"tiffTcl::getHeight path\"";
  int height;

  if ( objc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  TIFF* in = TIFFOpen( Tcl_GetStringFromObj( objv[ 1 ], 0 ), "r" );
  if ( ! in ) {
    Tcl_SetResult( interp, "error opening tiff image", TCL_VOLATILE );
    return TCL_ERROR;
  }

  TIFFGetField( in, TIFFTAG_IMAGELENGTH, &height );

  Tcl_SetObjResult( interp, Tcl_NewIntObj( height ) );

  TIFFClose( in );

  return TCL_OK;
}

static int tiffTclIsTiledProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  char * usage = "wrong # args: should be \"tiffTcl::isTiled path\"";

  if ( objc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  TIFF* in = TIFFOpen( Tcl_GetStringFromObj( objv[ 1 ], 0 ), "r" );
  if ( ! in ) {
    Tcl_SetResult( interp, "error opening tiff image", TCL_VOLATILE );
    return TCL_ERROR;
  }

  if ( TIFFIsTiled( in ) )
    Tcl_SetObjResult( interp, Tcl_NewIntObj( 1 ) );
  else
    Tcl_SetObjResult( interp, Tcl_NewIntObj( 0 ) );

  TIFFClose( in );

  return TCL_OK;
}

static int tiffTclGetTileWidthProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  char * usage = "wrong # args: should be \"tiffTcl::getTileWidth path\"";
  int tileWidth;

  if ( objc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  TIFF* in = TIFFOpen( Tcl_GetStringFromObj( objv[ 1 ], 0 ), "r" );
  if ( ! in ) {
    Tcl_SetResult( interp, "error opening tiff image", TCL_VOLATILE );
    return TCL_ERROR;
  }

  TIFFGetField( in, TIFFTAG_TILEWIDTH, &tileWidth );

  Tcl_SetObjResult( interp, Tcl_NewIntObj( tileWidth ) );

  TIFFClose( in );

  return TCL_OK;
}

static int tiffTclGetTileHeightProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  char * usage = "wrong # args: should be \"tiffTcl::getTileHeight path\"";
  int tileHeight;

  if ( objc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  TIFF* in = TIFFOpen( Tcl_GetStringFromObj( objv[ 1 ], 0 ), "r" );
  if ( ! in ) {
    Tcl_SetResult( interp, "error opening tiff image", TCL_VOLATILE );
    return TCL_ERROR;
  }

  TIFFGetField( in, TIFFTAG_TILELENGTH, &tileHeight );

  Tcl_SetObjResult( interp, Tcl_NewIntObj( tileHeight ) );

  TIFFClose( in );

  return TCL_OK;
}

static int tiffTclGetDescriptionProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  char * usage = "wrong # args: should be \"tiffTcl::getDescription path\"";
  char * desc = 0;

  if ( objc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  TIFF* in = TIFFOpen( Tcl_GetStringFromObj( objv[ 1 ], 0 ), "r" );
  if ( ! in ) {
    Tcl_SetResult( interp, "error opening tiff image", TCL_VOLATILE );
    return TCL_ERROR;
  }

  if (TIFFGetField( in, TIFFTAG_IMAGEDESCRIPTION, &desc ))
    Tcl_SetObjResult( interp, Tcl_NewStringObj( desc, -1 ) );

  TIFFClose( in );

  return TCL_OK;
}

/* lifted from vtiff3.c */
static int PrintKey(GeoKey *key, char **buf)
{
  char *data;
  int keyid = key->gk_key;
  int count = key->gk_count;
  int vals_now,i;
  pinfo_t *sptr;
  double *dptr;

  if (key->gk_type==TYPE_SHORT && count==1)
    data = (char *)&key->gk_data;
  else
    data = key->gk_data;
		
  switch (key->gk_type)
    {
    case TYPE_ASCII: 
      *buf = malloc (count + 50);
      strncpy (*buf, data, count);
      (*buf) [count-1] = '\0';	/* the last character is a '|', representing a null */
      if (strlen (*buf) > 75)
	(*buf)[76] = '\0';	/* zlinfo, zlget don't like long strings */
      sprintf (*buf + strlen(*buf), ";GK_KEY=%d", keyid);
      break;
    case TYPE_DOUBLE: 
      *buf = malloc (40 * count + 40);
      **buf = '\0';


      for (dptr = (double *)data; count > 0; count-= vals_now)
	{
	  vals_now = count > 3? 3: count;
	  for (i=0; i<vals_now; i++,dptr++)
	    {
	      if (**buf == '\0')
		sprintf (*buf, "(%.15lg", *dptr);
	      else
		sprintf (*buf + strlen (*buf), ", %.15lg", *dptr);
	    }
	  sprintf (*buf, ");GK_TYPE=%s;GK_KEY=%d", GTIFTypeName(key->gk_type), keyid);
	}
      break;
    case TYPE_SHORT: 
      sptr = (pinfo_t *)data;
      if (count==1)
	{
	  *buf = malloc(100);

	  sprintf (*buf,"%d(%s);GK_TYPE=%s;GK_KEY=%d", *sptr, GTIFValueName(keyid,*sptr), GTIFTypeName(key->gk_type), keyid);
	}
      else {
	*buf = malloc(15 * count + 50);
	**buf = '\0';

	for (; count > 0; count-= vals_now)
	  {
	    vals_now = count > 3? 3: count;
	    for (i=0; i<vals_now; i++,sptr++)
	      {
		if (**buf == '\0')
		  sprintf (*buf, "(%d", *sptr);
		else
		  sprintf (*buf + strlen (*buf), ", %d", *sptr);
	      }
	    sprintf (*buf, ");GK_TYPE=%s;GK_KEY=%d", GTIFTypeName(key->gk_type), keyid);
	  }
      }
      break;
    default: 
      *buf = malloc(100);
      sprintf (*buf, "Unknown Type (%d)\n", key->gk_type);
      break;
    }

  return keyid;
}

static int tiffTclGetGeoTiffTagsProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  char * usage = "wrong # args: should be \"tiffTcl::getGeoTiffTags path\"";

  if ( objc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  TIFF* in = TIFFOpen( Tcl_GetStringFromObj( objv[ 1 ], 0 ), "r" );

  if ( ! in ) {
    Tcl_SetResult( interp, "error opening tiff image", TCL_VOLATILE );
    return TCL_ERROR;
  }

  GTIF* gt = GTIFNew( in );

  if ( ! gt ) {
    TIFFClose( in );
    Tcl_SetResult( interp, "error creating geo key parser", TCL_VOLATILE );
    return TCL_ERROR;
  }

  int numkeys = gt->gt_num_keys;
  GeoKey *key = gt->gt_keys;
  int index;
  char * buf;
  int keyid;
  Tcl_Obj* *tags = (Tcl_Obj **) malloc (sizeof (Tcl_Obj *) * 2 * numkeys);

  for ( index = 0; index < numkeys; index ++ ) {
    buf = 0;
    keyid = PrintKey( ++key, &buf );

    tags[ 2 * index ] = Tcl_NewStringObj( GTIFKeyName( keyid ), -1 );
    tags[ 2 * index + 1 ] = Tcl_NewStringObj( buf, -1 );

    free( buf );
  }

  Tcl_SetObjResult( interp, Tcl_NewListObj( 2 * numkeys, tags ) );  

  free( tags );

  GTIFFree( gt );
  TIFFClose( in );

  return TCL_OK;
}

static int tiffTclGetGeoTiffTiepointsProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  char * usage = "wrong # args: should be \"tiffTcl::getGeoTiffTiepoints path\"";

  if ( objc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  TIFF* tif = TIFFOpen( Tcl_GetStringFromObj( objv[ 1 ], 0 ), "r" );

  if ( ! tif ) {
    Tcl_SetResult( interp, "error opening tiff image", TCL_VOLATILE );
    return TCL_ERROR;
  }

  GTIF* gt = GTIFNew( tif );

  if ( ! gt ) {
    TIFFClose( tif );
    Tcl_SetResult( interp, "error creating geo key parser", TCL_VOLATILE );
    return TCL_ERROR;
  }

  int doubleCount;
  double *data;

  if ( ( gt->gt_methods.get ) ( tif, GTIFF_TIEPOINTS, &doubleCount, &data ) ) {
    int sixTuple, part;
    int size = 3;		/* () and null */
    char * buf = malloc( size );
    *buf = '\0';
   
    for ( sixTuple = 0; sixTuple < doubleCount / 6; sixTuple ++ ) {
      char numBuf[ 40 ];

      for ( part = 0; part < 6; part ++ ) {
	sprintf( numBuf, "%.15lg", data[ sixTuple * 6 + part ]);
	size += strlen( numBuf ) + ( size == 3 ? 0 : 1 ); /* plus one for a comma */
	buf = realloc( buf, size );
	if ( size > 3 + strlen( numBuf ) )
	  strcat( buf, " " );
	strcat( buf, numBuf );
      }
    }

    Tcl_SetObjResult( interp, Tcl_NewStringObj( buf, -1 ) );

    free( buf );
  }

  GTIFFree( gt );
  TIFFClose( tif );

  return TCL_OK;
}

static int tiffTclGetGeoTiffPixelScaleProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  char * usage = "wrong # args: should be \"tiffTcl::getGeoTiffPixelScale path\"";

  if ( objc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  TIFF* tif = TIFFOpen( Tcl_GetStringFromObj( objv[ 1 ], 0 ), "r" );

  if ( ! tif ) {
    Tcl_SetResult( interp, "error opening tiff image", TCL_VOLATILE );
    return TCL_ERROR;
  }

  GTIF* gt = GTIFNew( tif );

  if ( ! gt ) {
    TIFFClose( tif );
    Tcl_SetResult( interp, "error creating geo key parser", TCL_VOLATILE );
    return TCL_ERROR;
  }

  int doubleCount;
  double *data;

  if ( ( gt->gt_methods.get ) ( tif, GTIFF_PIXELSCALE, &doubleCount, &data ) ) {
    int part;
    int size = 1;		/* just a null */
    char * buf = malloc( size );
    char numBuf [40];

    buf[0] = '\0';

    for ( part = 0; part < doubleCount; part ++ ) {
      sprintf( numBuf, "%.15lg", data[ part ]);
      size += strlen( numBuf ) + ( size == 1 ? 0 : 1 ); /* plus one for a space */
      buf = realloc( buf, size );
      if ( size > 1 + strlen( numBuf ) )
	strcat( buf, " " );
      strcat( buf, numBuf );
    }

    Tcl_SetObjResult( interp, Tcl_NewStringObj( buf, -1 ) );

    free( buf );
  }

  GTIFFree( gt );
  TIFFClose( tif );

  return TCL_OK;
}

static int tiffTclGetGeoTiffModelTransformationProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  char * usage = "wrong # args: should be \"tiffTcl::getGeoTiffModelTransformation path\"";

  if ( objc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  TIFF* tif = TIFFOpen( Tcl_GetStringFromObj( objv[ 1 ], 0 ), "r" );

  if ( ! tif ) {
    Tcl_SetResult( interp, "error opening tiff image", TCL_VOLATILE );
    return TCL_ERROR;
  }

  GTIF* gt = GTIFNew( tif );

  if ( ! gt ) {
    TIFFClose( tif );
    Tcl_SetResult( interp, "error creating geo key parser", TCL_VOLATILE );
    return TCL_ERROR;
  }

  int doubleCount;
  double *data;

  if ( ( gt->gt_methods.get ) ( tif, GTIFF_TRANSMATRIX, &doubleCount, &data ) ) {
    int part;
    int size = 3;		/* () and null */
    char * buf = malloc( 3 );
    char numBuf [40];

    buf[0] = '\0';

    for ( part = 0; part < doubleCount; part ++ ) {
      sprintf( numBuf, "%.15lg", data[ part ]);
      size += strlen( numBuf ) + ( size == 3 ? 0 : 1 ); /* plus one for a comma */
      buf = realloc( buf, size );
      if ( size > 3 + strlen( numBuf ) )
	strcat( buf, " " );
      strcat( buf, numBuf );
    }

    Tcl_SetObjResult( interp, Tcl_NewStringObj( buf, -1 ) );

    free( buf );
  }

  GTIFFree( gt );
  TIFFClose( tif );

  return TCL_OK;
}

static int tiffTclFileTypeProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  char * usage = "wrong # args: should be \"tiffTcl::fileType path\"";

  if ( objc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  FILE * infile = fopen (Tcl_GetStringFromObj( objv[ 1 ], 0 ), "r");
  if (! infile) {
    Tcl_SetResult( interp, "tiffTcl::isTiff: error opening file", TCL_VOLATILE );
    return TCL_ERROR;
  }

  /* check for TIFF */
  {
    char buf [10];

    if (fread (buf, 1, 10, infile) == 10 && /* see if it looks like a TIFF */
	((buf [0] == 0x49 &&
	  buf [1] == 0x49 &&
	  buf [2] == 0x2A &&
	  buf [3] == 0x00) ||
	 (buf [0] == 0x4D &&
	  buf [1] == 0x4D &&
	  buf [2] == 0x00 &&
	  buf [3] == 0x2A))) {

      TIFF *tif;
      GTIF *gt;
      int doubleCount;

      fclose (infile);

      /* open input TIFF file */
      if ((tif = TIFFOpen (Tcl_GetStringFromObj( objv[ 1 ], 0 ), "r"))) {
	pinfo_t *data;

	if ((gt = GTIFNew (tif)) &&
	    ((gt->gt_methods.get)(tif, GTIFF_GEOKEYDIRECTORY, &gt->gt_nshorts, &data) ||
	     (gt->gt_methods.get)(tif, GTIFF_TIEPOINTS, &doubleCount, &data) ||
	     (gt->gt_methods.get)(tif, GTIFF_PIXELSCALE, &doubleCount, &data) ||
	     (gt->gt_methods.get)(tif, GTIFF_TRANSMATRIX, &doubleCount, &data))) {
	  Tcl_SetObjResult( interp, Tcl_NewStringObj( "GEOTIFF", -1 ) ); /* it is a geotiff */
	} else {
	  Tcl_SetObjResult( interp, Tcl_NewStringObj( "TIFF", -1 ) ); /* it is a tiff */
	}

	GTIFFree (gt);
	TIFFClose (tif);

      } else {
	Tcl_SetObjResult( interp, Tcl_NewStringObj( "BADTIFF", -1 ) ); /* TIFFOpen failed */
	fclose (infile);
      }
    } else if (buf [0] == 'N' &&
	       buf [1] == 'I' &&
	       buf [2] == 'T' &&
	       buf [3] == 'F') {
      Tcl_SetObjResult( interp, Tcl_NewStringObj( "NITF", -1 ) ); /* it is an NITF */
      fclose (infile);
    } else if (! strncmp( buf, "LBLSIZE=", strlen( "LBLSIZE=" ) )) {
      Tcl_SetObjResult( interp, Tcl_NewStringObj( "VICAR", -1 ) ); /* it is a VICAR */
      fclose (infile);
    } else  {
      Tcl_SetObjResult( interp, Tcl_NewStringObj( "OTHER", -1 ) ); /* it's something else */
      fclose (infile);
    }
  }

  return TCL_OK;
}

int Tifftcl_Init( Tcl_Interp *interp ) {
  extern TIFFErrorHandler _TIFFwarningHandler;
  _TIFFwarningHandler = tiffTclWarningHandler;

  Tcl_CreateObjCommand( interp, "tiffTcl::getWidth", tiffTclGetWidthProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "tiffTcl::getHeight", tiffTclGetHeightProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "tiffTcl::isTiled", tiffTclIsTiledProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "tiffTcl::fileType", tiffTclFileTypeProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "tiffTcl::getTileWidth", tiffTclGetTileWidthProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "tiffTcl::getTileHeight", tiffTclGetTileHeightProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "tiffTcl::getDescription", tiffTclGetDescriptionProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "tiffTcl::tiffTclGetGeoTiffTags", tiffTclGetGeoTiffTagsProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "tiffTcl::tiffTclGetGeoTiffTiepoints", tiffTclGetGeoTiffTiepointsProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "tiffTcl::tiffTclGetGeoTiffPixelScale", tiffTclGetGeoTiffPixelScaleProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "tiffTcl::tiffTclGetGeoTiffModelTransformation", tiffTclGetGeoTiffModelTransformationProc, 0, 0 );

  return Tcl_PkgProvide( interp, "tiffTcl", "1");
}
