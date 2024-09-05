/*  16Jul2003   gmg
*   Header file for the Digital Elevation Map input program
*/
#ifndef DEM_HEADER

#define DEM_HEADER

#define DEM_PATH    "DTED//"    // path to DEM files
#define ELOFF            32768  // elevation offset (meters)
#define LATTILE           1200  // number of latitude points in a tile
#define MAXDEMN     5598720000  // max DEM points in playbox (432 square deg)
#define NAMEHIST    "dem.hst"   // name of DEM histogram output file
#define NAMEINPUT   "dem.dat"   // name of parameter input file
#define NAMELOG     "dem.log"   // name of log output file
#define SEPARATOR   "/"
#define Index(x, y) ((Point)(x) + (Point)glonn * (Point)(y))
#define Lat(y)  ((double)((y) * glatd + glata) / 3600.)
#define Lon(x)  ((double)((x) * glond + glona) / 3600.)
#define lower(x) ((x) < 0 ? 3600*((int)(((x)+1)/3600)-1) : 3600*(int)((x)/3600))
#define upper(x) ((x) > 0 ? 3600*((int)(((x)-1)/3600)+1) : 3600*(int)((x)/3600))
#define max(x, y)   ((x) > (y) ? (x) : (y))
#define min(x, y)   ((x) < (y) ? (x) : (y))
#define ulat(x) ((x) < 0 ? 'S' : 'N')
#define ulon(x) ((x) < 0 ? 'W' : 'E')

#include    <math.h>
#include    <stdio.h>
#include    "Quadtree.h"
#include    <stdlib.h>
#include    "DataSource.h"

typedef Elevation       Elev;
typedef long int        Point;

  extern FILE          *fl;            // FILE pointer for log file
  extern int           gbigend;    // bigendian = 1, little-endian = 0
  extern double        gc[7];          // coefficients in elevation equation
  extern char          gDemDir[80];    // global DEM directory name
  extern double        gearthr;        // radius of the Earth (meters)
  extern Elev          *gel;           // elevation (meters)
  extern int           gelmax;         // maximum allowed elevation
  extern int           gelmin;         // minimum allowed elevation
  extern Angle         gLatBot;        // latitude, bottom, playbox (arc seconds)
  extern Angle         gLatTop;        // latitude,    top, playbox (arc seconds)
  extern Angle         gLonLft;        // longitude,  left, playbox (arc seconds)
  extern Angle         gLonRgt;        // longitude, right, playbox (arc seconds)
  extern Angle         glata;          // latitude , absolute (arc seconds)
  extern Angle         glona;          // longitude, absolute (arc seconds)
  extern Angle         glatd;          // latitude , delta    (arc seconds)
  extern Angle         glond;          // longitude, delta    (arc seconds)
  extern int           glatn;          // latitude , number of points
  extern int           glonn;          // longitude, number of points
  extern int           glonpnts;       // number of longitudinal points in playbox
  extern Point         gndem;          // number of points in DEM array

int ReadDem();

#endif


