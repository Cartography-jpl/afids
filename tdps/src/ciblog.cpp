#include <ossimFilename.h>
#include <ossimImageHandlerFactory.h>
#include <ossimCibCadrgTileSource.h>
#include <ossimImageHandler.h>
#include <ossimBandSelector.h>
#include <ossimImageSourceSequencer.h>

extern "C" {
#include "zvproto.h"
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

static int cibToVicar( char * inPath, char * outPath, double n, double e, double w, double s ) {
  const int image = 0;
  const int band = 0;

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

  int horizTiles = iss.getNumberOfTilesHorizontal();
  int vertTiles = iss.getNumberOfTilesVertical();
  int tileWidth = iss.getTileWidth();
  int tileHeight = iss.getTileHeight();
  int sourceNs = horizTiles * tileWidth;
  int sourceNl = vertTiles * tileHeight;

  ossimKeywordlist keywordList;
  if (! ih->getImageGeometry( keywordList ) ) {
    cerr << "Failed to get image geometry" << endl;
    free( inPath );
    free( outPath );
    return 1;
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
    cerr << "Failed extracting image corners" << endl;
    free( inPath );
    free( outPath );
    return 1;
  }
  double imageLonWidth = d_ur_lon - d_ul_lon;
  double imageLatHeight = d_ul_lat - d_ll_lat;
  double xScale = imageLonWidth / sourceNs;
  double yScale = imageLatHeight / sourceNl;

  if (n < d_ll_lat || e < d_ul_lon || w > d_ur_lon || s > d_ul_lat) {
    cerr << "Requested area outside CIB data:" << endl;
    cerr << "Requested n " << n << " e " << e << " w " << w << " s " << s << endl;
    cerr << "CIB       n " << ul_lat << " e " << ur_lon << " w " << ul_lon << " s " << ll_lat << endl;
    return 1;
  }

  if (n > d_ul_lat)
    n = d_ul_lat;
  if (e > d_ur_lon)
    e = d_ur_lon;
  if (w < d_ul_lon)
    w = d_ul_lon;
  if (s < d_ll_lat)
    s = d_ll_lat;

  int sl, ss, el, es, nl, ns;
  
  ss = int (sourceNs * (w - d_ul_lon) / imageLonWidth);
  sl = int (sourceNl * (d_ul_lat - n) / imageLatHeight);
  es = int (sourceNs * (e - d_ul_lon) / imageLonWidth) - 1; // e is lon of right edge of last pixel
  el = int (sourceNl * (d_ul_lat - s) / imageLatHeight) - 1; // s is lat of bottom edge of last pixel
  ns = es - ss + 1;
  nl = el - sl + 1;
  sl ++;			// one-based from here
  ss ++;			// one-based from here

  double ulLon = d_ul_lon + imageLonWidth * ((double) ss) / sourceNs;
  double ulLat = d_ul_lat - imageLatHeight * ((double) sl) / sourceNl;

  int vunit;
  if (zvunit( & vunit, "U_NAME", 1, "U_NAME", (const char *) outPath, 0 ) != 1) {
    cerr << "zvunit failed on outPath" << endl;
    free( inPath );
    free( outPath );
    return 1;
  }

  if (zvopen( vunit, "U_NL", nl, "U_NS", ns, "OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA", 0 ) != 1) {
    cerr << "zvopen failed on outPath" << endl;
    free( inPath );
    free( outPath );
    return 1;
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
  */

  char tiePoint[100];
  sprintf (tiePoint, "(0,0,0,%lf,%lf,0)", ulLon, ulLat);
  char scale[100];
  sprintf (scale, "(%lf,%lf,0)", xScale, yScale);

  if (addStringToProperty ("MODELTIEPOINTTAG", tiePoint, 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("MODELPIXELSCALETAG", scale, 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GTMODELTYPEGEOKEY", "2(ModelTypeGeographic);gk_type=Short;gk_key=1024", 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GTRASTERTYPEGEOKEY", "1(RasterPixelIsArea);gk_type=Short;gk_key=1025", 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GTCITATIONGEOKEY", "Extracted by ossimTcl;gk_key=1026", 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GEOGRAPHICTYPEGEOKEY", "4326(GCS_WGS_84);gk_type=Short;gk_key=2048", 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GEOGLINEARUNITSGEOKEY", "9001(Linear_Meter);gk_type=Short;gk_key=2052", 1, & vunit, "GEOTIFF", 0) ||
      addStringToProperty ("GEOGANGULARUNITSGEOKEY", "9102(Angular_Degree);gk_type=Short;gk_key=2054", 1, & vunit, "GEOTIFF", 0)) {
    cerr << "addStringToProperty failed on outPath" << endl;
    free( inPath );
    free( outPath );
    return 1;
  }

  int stripLoaded = -1;
  ossimImageData * imageData = 0;
  for (int line = sl; line < sl + nl; line ++) {
    int strip = (line - 1) / tileHeight;
    if (strip != stripLoaded) {
      ossimIrect oneStrip( 0, strip * tileHeight, sourceNs - 1, ((strip + 1) * tileHeight) - 1 );
      imageData = iss.getTile( oneStrip );
      stripLoaded = strip;
    }

    if ((line - sl) % (nl / 10) == 0)
      cout << "Writing line " << line - sl << "/" << nl << endl;

    if (zvwrit (vunit, (((char *) imageData->getBuf()) + sourceNs * ((line - 1) % tileHeight) + ss - 1), "LINE", line - sl + 1, "SAMP", 1, "NSAMPS", ns, 0) != 1) {
      cerr << "zvwrit failed on outPath" << endl;
      free( inPath );
      free( outPath );
      return 1;
    }
  }

  zvclose (vunit, 0);

  return 0;
}

static int cibToVicar( char * inPath, char * outPath, double n, double e, double w, double s );

int main ( int argc, char * argv [] ) {
  char * usage = "usage: ciblog pathTo_a.toc outPath n e w s";

  if (argc != 7) {
    cerr << usage << endl;
    return 1;
  }

  double n, e, w, s;

  if (sscanf( argv[ 3 ], "%lf", & n ) != 1 ||
      sscanf( argv[ 4 ], "%lf", & e ) != 1 ||
      sscanf( argv[ 5 ], "%lf", & w ) != 1 ||
      sscanf( argv[ 6 ], "%lf", & s ) != 1) {
    cerr << "error parsing lat or lon from n, e, w, s" << endl;
    return 1;
  }

  if (cibToVicar( argv[ 1 ], argv[ 2 ], n, e, w, s )) {
    cerr << "cibToVicar returned error status" << endl;
    return 1;
  }

  return 0;
}
