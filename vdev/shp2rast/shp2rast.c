#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdlib.h>

#include "defines.h"
#include "vicmain_c.h"
#include "zifmessage.h"
#include "zmabend.h"
#include "carto/shapefil.h"
#include "zvprintf.h"
#include "gems.h"
#include "concave.h"

static char *cell = NULL;
static int pixPerDeg = 0;
static int margin = 0;

#define halfPixelWidthInDegrees (1.0 / (2.0 * pixPerDeg))

#ifndef MAX
#define MAX(a,b) ((a)>(b)?(a):(b))
#endif

static int maxdn = 0;
static int rasterColor = 0;
void drawProc(int y, int xl, int xr) {
  /* used by concave(), which checks cell boundaries, so don't need to check here */
  int index = y * (pixPerDeg + margin) + xl;
  int width = (xr - xl + 1);
  if (maxdn) {
    int i;
    for (i = 0; i < width; ++i)
      if (cell[index + i] < rasterColor)
	cell[index + i] = rasterColor;
  } else
    memset (cell + index, rasterColor, sizeof (char) * width);
}

static void dotproc(int x, int y) {
  /* if x, y in cell */
  if (x >=0 && y >= 0 && x < (pixPerDeg + margin) && y < (pixPerDeg + margin)) {
    int index = y * (pixPerDeg + margin) + x;

    if (maxdn)
      cell[index] = MAX(rasterColor, cell[index]);
    else
      cell[index] = rasterColor;
  }
}

void drawLine(int nvert,		/* number of vertices */
	      Point2 *points) {		/* vertices of polygon */
  int vert, x1, x2, y1, y2;

  for (vert = 0; vert < nvert - 1; ++vert) {
    x1 = points[vert].x;
    y1 = points[vert].y;
    x2 = points[vert + 1].x;
    y2 = points[vert + 1].y;
    digline(x1, y1, x2, y2, dotproc);
  }
}

typedef struct {
  char *value;
  int dn;
} ValueDn;

static void addGTLabel (int vunit, char * key, char * value) {
  char msgBuf [1000];
  int status;

  if ((status = zladd (vunit, "PROPERTY",
		       key, value,
		       "PROPERTY", "GEOTIFF",
		       "FORMAT", "STRING", NULL)) != 1) {
    snprintf (msgBuf, 200, "addGTLabel failed to add a label for key %s, value %s, status %d", key, value, status);
    zmabend (msgBuf);
  }
}

void main44(void)
{
  int parmct, parmdf;
  char inpfilename [99];
  char outfileprefix [99];
  char outfilesuffix [99];
  char mapfeat[99];
  char mapfile[99];
  int vunit;
  int valueDnListLen = 0;
  ValueDn *valueDnList = NULL;

  SHPHandle shpHandle;
  DBFHandle dbfHandle = NULL;
  int nEntities, nShapetype;
  double xyzmMin [4], xyzmMax [4];

  char dataPresent [360 * 181];

  int entity;
  int vertex;
  int i, j;

  SHPObject * shpObject;
  int maxPoints = 0;		/* max number of points in a polygon for the dataset */

  int minLat, maxLat, minLon, maxLon;
  int fg, bg, vecFg;
  int nofill;
  int dbfIndex = 0;
  int fillExtent = 0;
  char geoType[6];
  int cnt;

  zifmessage ("SHP2RAST version 2019-08-15");

  /* fetch params */
  zvparm ("shp", inpfilename, &parmct, &parmdf, 1, 99);
  zvparm ("prefix", outfileprefix, &parmct, &parmdf, 1, 99);
  zvparm ("suffix", outfilesuffix, &parmct, &parmdf, 1, 99);
  zvp ("minLat", & minLat, & parmct);
  zvp ("maxLat", & maxLat, & parmct);
  zvp ("minLon", & minLon, & parmct);
  zvp ("maxLon", & maxLon, & parmct);
  zvp ("fg", & fg, & parmct);
  zvp ("bg", & bg, & parmct);
  zvp ("pixPerDeg", & pixPerDeg, & parmct);
  zvparm ("mapfeat", mapfeat, &parmct, &parmdf, 1, 99);
  zvparm ("mapfile", mapfile, &parmct, &parmdf, 1, 99);
  margin = zvptst("margin");
  nofill = zvptst("nofill");
  maxdn = zvptst("maxdn");
  fillExtent = zvptst("fillextent");
  zvp("geotype", geoType, &cnt);

  /* Read mapfile if available, and open dbf file */
  if (strlen(mapfile) > 0 && strlen(mapfeat) > 0) {
    FILE *f;
    int lineBufSize = 50;
    char line[lineBufSize];
    int dn;
    char* comma;

    zvnprintf(50, "filtering on feature %s", mapfeat);

    if (! (f = fopen(mapfile, "rb")))
      zvnabend(150, "error opening mapfile %s", mapfile);
    while (!feof(f)) {
      if (fgets(line, lineBufSize, f)) {
	if (! (comma = strchr(line, ',')))
	  zvnabend(150, "error parsing comma from mapfile %s", mapfile);
	if (sscanf(comma+1, "%d", &dn) != 1)
	  zvnabend(150, "error parsing int from mapfile %s value %s", mapfile, comma);
	*comma = '\0';

	++ valueDnListLen;
	valueDnList = (ValueDn*) realloc(valueDnList, sizeof(ValueDn) * valueDnListLen);
	if (! valueDnList)
	  zvnabend(150, "error mallocing while parsing mapfile %s", mapfile);
	valueDnList[valueDnListLen-1].value = strdup(line);
	valueDnList[valueDnListLen-1].dn = dn;
      }

      if (! (dbfHandle = DBFOpen (inpfilename, "rb")))
	zvnabend(1000, "error opening %s dbf for input", inpfilename);
      if ((dbfIndex = DBFGetFieldIndex(dbfHandle, mapfeat)) < 0)
	zvnabend(1000, "error getting dbf index from %s", inpfilename);
    }

    /* for (i = 0; i < valueDnListLen; ++i) */
    /*   zvnprintf(100, "Feature %s, dn %d", valueDnList[i].value, valueDnList[i].dn); */

    fclose(f);
  }

  cell = (char *) malloc(sizeof(char) * (pixPerDeg + margin) * (pixPerDeg + margin));

  if (minLat < -90 || minLat > 90 ||
      maxLat < -90 || maxLat > 90 ||
      minLat >= maxLat ||
      minLon < -180 || minLon > 180 ||
      maxLon < -180 || maxLon > 180 ||
      minLon >= maxLon)
    zmabend ("lats must be in range -90 to 90; lons must be in range -180 to 180; mins must be less than maxs");      
  minLat += 90;
  maxLat += 90;			/* 89, because it will be used as an index */
  minLon += 180;
  maxLon += 180;		/* ditto */

  /* open shape input */
  if (! (shpHandle = SHPOpen (inpfilename, "rb")))
    zvnabend(1000, "error opening %s for input", inpfilename);
  
  SHPGetInfo (shpHandle, & nEntities, & nShapetype, xyzmMin, xyzmMax);
  zvnprintf (1000, "shp2rast: nEntities==%d", nEntities);

  /* check shape type */
  switch (nShapetype) {
  case SHPT_NULL:
    zvnabend (1000, "%s contains an unsupported shape entity type: NULL", inpfilename);
  case SHPT_POINT:
    zvnabend (1000, "%s contains an unsupported shape entity type: POINT", inpfilename);
  case SHPT_ARC:
  case SHPT_POLYGON:
    break;
  case SHPT_MULTIPOINT:
    zvnabend (1000, "%s contains an unsupported shape entity type: MULTIPOINT", inpfilename);
  case SHPT_POINTZ:
    zvnabend (1000, "%s contains an unsupported shape entity type: POINTZ", inpfilename);
  case SHPT_ARCZ:
    zvnabend (1000, "%s contains an unsupported shape entity type: ARCZ", inpfilename);
  case SHPT_POLYGONZ:
    zvnabend (1000, "%s contains an unsupported shape entity type: POLYGONZ", inpfilename);
  case SHPT_MULTIPOINTZ:
    zvnabend (1000, "%s contains an unsupported shape entity type: MULTIPOINTZ", inpfilename);
  case SHPT_POINTM:
    zvnabend (1000, "%s contains an unsupported shape entity type: POINTM", inpfilename);
  case SHPT_ARCM:
    zvnabend (1000, "%s contains an unsupported shape entity type: ARCM", inpfilename);
  case SHPT_POLYGONM:
    zvnabend (1000, "%s contains an unsupported shape entity type: POLYGONM", inpfilename);
  case SHPT_MULTIPOINTM:
    zvnabend (1000, "%s contains an unsupported shape entity type: MULTIPOINTM", inpfilename);
  case SHPT_MULTIPATCH:
    zvnabend (1000, "%s contains an unsupported shape entity type: MULTIPATCH", inpfilename);
  default:
    zvnabend (1000, "%s contains an unsupported shape entity type: UNKNOWN", inpfilename);
  }

  if (nShapetype != SHPT_POLYGON && nShapetype != SHPT_ARC)
    zmabend ("Input must contain either POLYGONs or ARCs");

  /* determine which 1x1 degree cells have data */
  memset (dataPresent, 0, sizeof (char) * 360 * 181);
      
  /* for each polygon ... */
  for (entity = 0; entity < nEntities; entity ++) {
    shpObject = SHPReadObject (shpHandle, entity);

    if (! shpObject)
      zmabend ("SHPReadObject failed");

    /* keep track of largest polygon size for later */
    maxPoints = (maxPoints < shpObject -> nVertices) ? shpObject -> nVertices : maxPoints;

    /* for each vertex ... */
    for (vertex = 0; vertex < shpObject -> nVertices; vertex ++) {
      double tmp;

      /* calculate the vertex's dataPresent x coordinate */
      tmp = shpObject -> padfX [vertex]
	/* Need to shift values to a positive coordinate
	   space for rasterizing algorithm*/
	+ 180.0;

      i = (int) tmp;

      if (i == 360) {
	i = 0;
      }

      if (i == -1) {
	i = 359;
      }

      if (i < -1 || i > 360)
	zvnabend (1000, "index %d computed from lon %f", i, shpObject -> padfX [vertex]);
	     
      /* calculate the vertex's dataPresent y coordinate */
      tmp = shpObject -> padfY [vertex]
	+ 90.0;

      j = (int) tmp;

      if (j < 0 || j > 180)
	zvnabend (1000, "index %d computed from lat %f", j, shpObject -> padfY [vertex]);
	     
      dataPresent [i + j * 360] = 1;
    }

    SHPDestroyObject (shpObject);
  }

  /* find smallest rectangle containing all dataPresent and count occupied cells */
  int minI = 361;
  int maxI = -1;
  int minJ = 181;
  int maxJ = -1;
  {
    int cellCount = 0;

    for (i = minLon; i < maxLon; i ++)
      for (j = minLat; j < maxLat; j ++)
	if (dataPresent [i + j * 360]) {
	  minI = (minI > i ? i : minI);
	  maxI = (maxI < i ? i : maxI);
	  minJ = (minJ > j ? j : minJ);
	  maxJ = (maxJ < j ? j : maxJ);
	  cellCount ++;
	}

    if (! cellCount)
      zvnabend (1000, "%s is completely outside the specified area", inpfilename);

    zvnprintf (1000, "shp2rast: %s touches %d cells with lon range %d:%d, lat range %d:%d", inpfilename, cellCount, minI - 180, maxI - 179, minJ - 90, maxJ - 89);
  }

  {
    char outFileName [1000];
    Point2 * points = (Point2 *) malloc (sizeof (Point2) * maxPoints);
      
    /* rasterize shape data into each cell image */
    for (i = minI; i <= maxI; i ++)
      for (j = minJ; j <= maxJ; j ++)
	if (dataPresent [i + j * 360] || fillExtent) {
	  /* erase the rasterizing cell */
	  memset (cell, bg, sizeof (char) * (pixPerDeg + margin) * (pixPerDeg + margin));

	  /* compute file name */
	  snprintf (outFileName, 1000, "%s%c%02d%c%03d%s", outfileprefix,
		    (j - 90) < 0 ? 's' : 'n', abs (j - 90),
		    (i - 180) < 0 ? 'w' : 'e', abs (i - 180),
		    outfilesuffix);

	  /* check dataPresent again, perhaps we are here because we are just generating an empty cell */
	  if (dataPresent [i + j * 360]) {
	      /* for each polygon, if it touches this cell, rasterize it */
	      for (entity = 0; entity < nEntities; entity ++) {
		shpObject = SHPReadObject (shpHandle, entity);

		vecFg = fg;

		/* set fg based on entity and dn map if available */
		if (valueDnList) {
		  int dnListIndex;
		  const char *attr = DBFReadStringAttribute(dbfHandle, entity, dbfIndex);

		  if (!attr)
		    zvnabend(100, "error getting attribute for entity %d, index %d", entity, dbfIndex);

		  for (dnListIndex = 0; dnListIndex < valueDnListLen; ++dnListIndex)
		    if (!strcmp(attr, valueDnList[dnListIndex].value)) {
		      vecFg = valueDnList[dnListIndex].dn;
		      break;
		    }
		}

		{
		  int vi, vj;
		  double tmp;

		  for (vertex = 0; vertex < shpObject -> nVertices; vertex ++) {
		    /* calculate the vertex's dataPresent x coordinate */
		    tmp = shpObject -> padfX [vertex]
		      /* Need to shift values to a positive coordinate
			 space for rasterizing algorithm*/
		      + 180.0;

		    vi = (int) tmp;

		    if (vi == 360)
		      vi = 0;

		    if (vi == -1)
		      vi = 359;
	     
		    /* calculate the vertex's dataPresent y coordinate */
		    tmp = shpObject -> padfY [vertex]
		      + 90.0;

		    vj = (int) tmp;

		    if (vi == i && vj == j)
		      break;
		  }

		  /* if we left the loop early, then we found a vertex in the cell, so rasterize it */
		  if (vertex < shpObject -> nVertices) {
		    int numPoints = 0;
		    int part;
		    Window cellBoundaries = {0, 0, pixPerDeg - 1 + margin, pixPerDeg - 1 + margin};

		    /* collect points into cell space coordinates for concave function */
		    for (vertex = 0, part = 0; vertex < shpObject -> nVertices; vertex ++) {
		      if (part < shpObject -> nParts && shpObject -> panPartStart [part] == vertex) {
			if (part > 0) { /* this is the first vertex of a hole */
			  if (part == 1) /* rasterize polygon (the previous part) */
			    rasterColor = vecFg;
			  else	/* rasterize hole */
			    rasterColor = bg;
		      
			  /* start next part */
			  numPoints = 0;
			}

			part ++;
		      }

		      points [numPoints] . x = (shpObject -> padfX [vertex]
						/* Need to shift values to a positive coordinate space */
						+ 180.0
						/* subtract left edge of 1 degree cell; this should leave >= 0, < 1 degree */
						- i)
			* pixPerDeg;

		      points [numPoints] . y = (shpObject -> padfY [vertex]
						+ 90.0
						- j)
			* pixPerDeg + margin;

		      /* if this is the first point, or it is a point different from the last point, increment numPoints */
		      if (! numPoints ||
			  points [numPoints] . x != points [numPoints - 1] . x ||
			  points [numPoints] . y != points [numPoints - 1] . y) {
			numPoints ++;
		      }
		    }

		    /* rasterize the points */
		    if (part > 1)
		      rasterColor = bg;
		    else
		      rasterColor = vecFg;

		    /* the concave function should but does not draw the enclosing line, just the fill, so always draw the line */
		    drawLine (numPoints, points);
		    if (! nofill)
		      concave (numPoints, points, & cellBoundaries, drawProc);
		  }

		  SHPDestroyObject (shpObject);
		}
	      }
	  }

	  zvnprintf (1000, "shp2rast: creating %s", outFileName);

	  /* create output image */
	  if (zvunit (& vunit, "U_NAME", 1, "U_NAME", outFileName, NULL) != 1)
	    zvnabend (1000, "zvunit failed on %s", outFileName);

	  if (zvopen (vunit, "U_NL", pixPerDeg + margin, "U_NS", pixPerDeg + margin, "OP", "WRITE", "OPEN_ACT", "SA", NULL) != 1)
	    zvnabend (1000, "zvopen failed on %s", outFileName);

	  /* add GeoTIFF label if requested, either pixel is point or area */
	  if (strcmp(geoType, "NOGEO")) {
	    char buf[100];
	    double scale = 1.0 / (double) pixPerDeg;

	    snprintf(buf, 100, "(%lg,%lg,0.0)", scale, scale);
	    addGTLabel (vunit, "MODELPIXELSCALETAG", buf);
	    addGTLabel (vunit, "GTMODELTYPEGEOKEY", "2(ModelTypeGeographic)");
	    addGTLabel (vunit, "GEOGANGULARUNITSGEOKEY", "9102(Angular_Degree)");
	    addGTLabel (vunit, "GEOGRAPHICTYPEGEOKEY", "4326(GCS_WGS_84)");
	    addGTLabel (vunit, "GEOGELLIPSOIDGEOKEY", "7030(Ellipse_WGS84)");

	    if (!strcmp(geoType, "AREA")) {
	      addGTLabel (vunit, "GTRASTERTYPEGEOKEY", "1(RasterPixelIsArea)");
	      snprintf(buf, 100, "(0.5,0.5,0,%d,%d,0)", i - 180, j - 89);
	      addGTLabel (vunit, "MODELTIEPOINTTAG", buf);
	    } else if (!strcmp(geoType, "POINT")) {
	      addGTLabel (vunit, "GTRASTERTYPEGEOKEY", "2(RasterPixelIsPoint)");
	      snprintf(buf, 100, "(0,0,0,%d,%d,0)", i - 180, j - 89);
	      addGTLabel (vunit, "MODELTIEPOINTTAG", buf);
	    }
	  }

	  {
	    int line;

	    for (line = (pixPerDeg + margin) - 1; line >= 0; line --) /* flip vertically */
	      /* write one image line to VICAR cell output */
	      zvwrit (vunit, cell + line * (pixPerDeg + margin), "LINE", (pixPerDeg + margin) - line, "SAMP", 1, "NSAMPS", pixPerDeg + margin, NULL);
	  }

	  /* done with VICAR cell image */
	  zvclose (vunit, NULL);
	}
  }

  if (dbfHandle)
    DBFClose (dbfHandle);
  SHPClose (shpHandle);

  return;
}
