/*  15jul2003   gmg
*   Routines to read Digital Elevation Maps for the
*   Line of Sight program.
*/

#include "Dem.h"


  FILE          *fl;            // FILE pointer for log file
  int           gbigend = 1;    // bigendian = 1, little-endian = 0
  double        gc[7];          // coefficients in elevation equation
  char          gDemDir[80];    // global DEM directory name
  double        gearthr;        // radius of the Earth (meters)
  Elev          *gel;           // elevation (meters)
  int           gelmax;         // maximum allowed elevation
  int           gelmin;         // minimum allowed elevation
  Angle         gLatBot;        // latitude, bottom, playbox (arc seconds)
  Angle         gLatTop;        // latitude,    top, playbox (arc seconds)
  Angle         gLonLft;        // longitude,  left, playbox (arc seconds)
  Angle         gLonRgt;        // longitude, right, playbox (arc seconds)
  Angle         glata;          // latitude , absolute (arc seconds)
  Angle         glona;          // longitude, absolute (arc seconds)
  Angle         glatd;          // latitude , delta    (arc seconds)
  Angle         glond;          // longitude, delta    (arc seconds)
  int           glatn;          // latitude , number of points
  int           glonn;          // longitude, number of points
  int           glonpnts;       // number of longitudinal points in playbox
  Point         gndem;          // number of points in DEM array


static int TDPS_flag = 0;



/***********************************************************************
*   Writelog: writes a string to the log file.
*/

void WriteLog
(
  char          sl[]            // character string to print to logfile
)
{
  FILE   *fl;                   // FILE pointer to log file

  if((fl = fopen(NAMELOG, "a")) == NULL)
  {
    printf("Cannot open logfile %s\n", NAMELOG);
    exit(1);
  }
  fprintf(fl, "%s", sl);
  fclose(fl);
  return;
}

/***********************************************************************
*   OpenDem: opens the requested Digital Elevation Map file.
*   Returns the file pointer if the file was found;
*   and NULL if the file was not found.
*/

FILE *OpenDem
(
  int           la,             // latitude  (degrees) of DEM file
  int           lo              // longitude (degrees) of DEM file
)
{
  char          filename[80];   // complete filename of DEM file
  extern char   gDemDir[80];    // global DEM directory name
  char          sl[81];         // string for log file messages
int i;
char temp_buf[256];
FILE *fd = NULL;

//  Create the file name and open the file
//  sprintf(filename, "%s%s%c%03d%s%c%02d.DT1", gDemDir, DEM_PATH,
if ( !TDPS_flag )
{
  sprintf(temp_buf, "%c%03d%s%c%02d.DT1",  
          ulon(lo), abs(lo), SEPARATOR, ulat(la), abs(la));
  sprintf(filename, "%s%s", gDemDir,temp_buf); 

  sprintf(sl, "OpenDem:    Attempt to open %s\n", filename);
  WriteLog(sl);

  if ( (fd = fopen(filename, "rb")) == NULL )
  {
    for ( i=0 ; temp_buf[i] ; i++ )        // convert name to lower case
      temp_buf[i] = tolower(temp_buf[i]);  

    sprintf(filename, "%s%s", gDemDir,temp_buf);

    sprintf(sl, "OpenDem:    Attempt to open %s\n", filename);
    WriteLog(sl);

    fd = fopen(filename, "rb");
  }
}
  if ( TDPS_flag || (fd == NULL) )    // try for TDPS files
  {
    char ew = tolower(ulon(lo));
    char ns = tolower(ulat(la));
    sprintf(temp_buf, "%c%d%c%d_L1.hlf", ns, abs(la), ew, abs(lo));
    sprintf(filename, "%s%s", gDemDir,temp_buf);

    sprintf(sl, "OpenDem:    Attempt to open %s\n", filename);
    WriteLog(sl);
    if ( fd = fopen(filename, "rb") )
    {
      TDPS_flag = 1;
    }
  }
  return(fd);
}


/***********************************************************************
*   InputAngle: reads in an angle in either degrees, minutes, and
*   seconds or integer degrees.
*   Returns the angle in arc seconds.
*/

int InputAngle
(
  char          line[]          // one line of characters from input
)
{
  int           deg;            // input degrees
  int           min = 0;        // input minutes
  int           sec = 0;        // input seconds

  sscanf(line, "%*s%d:%d:%d", &deg, &min, &sec);
  if(deg < 0)
    return(deg * 3600 - min * 60 - sec);
  else
    return(deg * 3600 + min * 60 + sec);
}

/***********************************************************************
*   StatDem: Gathers histogram data from elevation array.
*   Returns the number of elevation points.
*/

Point StatDem()
{
  int           elmax = -ELOFF; // maximum elevation
  int           elmin = -ELOFF; // minimum elevation
  FILE          *fh;            // FILE pointer to DEM statistics file
  extern Elev   *gel;           // elevation (meters)
  extern Point  gndem;          // number of points in DEM array
  int           ih;             // index of histogram bin
  Point         ip;             // index of elevation point
  Point         nele = 0;       // number of elevation points read
  Point         nppe[ELOFF*2];  // number of points per elevation array
  char          sl[81];         // string for log file messages

  if((fh = fopen(NAMEHIST, "w")) == NULL)
  {
    printf (    "StatDem:    Cannot open output file     %s\n", NAMEHIST);
    sprintf(sl, "StatDem:    Cannot open output file     %s\n", NAMEHIST);
    WriteLog(sl);
    exit(1);
  }
  for(ih = 0; ih < ELOFF * 2; ih++)
    nppe[ih] = 0;
  for(ip = 0; ip < gndem; ip++)
    nppe[gel[ip] + ELOFF]++;
  for(ih = 0; ih < ELOFF * 2; ih++)
    if(nppe[ih] != 0)
    {
      nele += nppe[ih];
      elmax = ih - ELOFF;
      if(elmin == -ELOFF)
        elmin = ih - ELOFF;
    }
  fprintf(fh, " nele:%10lld    elmin:%6d    elmax:%6d\n", nele, elmin, elmax);
  fprintf(fh, "    Elevation   number of points\n");
  for(ih = 0; ih < ELOFF * 2; ih++)
    if(nppe[ih] != 0)
      fprintf(fh, "     %6d      %10lld\n", ih - ELOFF, nppe[ih]);
  fclose(fh);
  return(nele);
}

/***********************************************************************
*   Decode: converts binary data from DEM file into short integer format
*   depending on endian.
*   Returns the short integer data.
*/

Elev Decode
(
  Elev          data            // binary elevation data from DEM file
)
{
  extern int        gbigend;    // bigendian = 1, little-endian = 0
  unsigned short    data2;      // temporary data storage

  if(!gbigend)
  {
    data2 = (unsigned short) data;
    data = (Elev)((data2 << 8) | (data2 >> 8));
    if ( (data < 0) && (!TDPS_flag) )
      data = -(data & 0x7FFF);
  }
  return(data);
}

/***********************************************************************
*   ReadFile: reads one Digital Elevation Map file for one tile.
*   NOTE: South and West are negative.
*   This currently handles DTED1 files and will be modified at some
*   future date to handle DTED2 files as well.
*   Returns 1 if the elevation array was filled and -1 if it was not.
*/

int ReadFile
(
  int           latile,         // bottom latitude of tile (arc seconds)
  int           lotile          // left  longitude of tile (arc seconds)
)
{
  int           el;             // elevation (meters)
  FILE          *fd;            // FILE pointer to open DEM file
  extern double gc[7];          // coefficients in elevation equation
  extern Elev   *gel;           // elevation (meters)
  extern int    gelmax;         // maximum allowed elevation
  extern int    gelmin;         // minimum allowed elevation
  extern Angle  glata;          // latitude,  absolute (arc seconds)
  extern Angle  glona;          // longitude, absolute (arc seconds)
  extern Angle  glatd;          // latitude,  delta    (arc seconds)
  extern Angle  glond;          // longitude, delta    (arc seconds)
  extern int    glatn;          // latitude,  number of points
  extern int    glonn;          // longitude, number of points
  short int     ida[3601];      // integer data array
  int           ila;            // index to  latitude of elevation point
  int           ilo;            // index to longitude of elevation point
  int           ilabot;         // index to  latitude, bottom, within this tile
  int           ilatop;         // index to  latitude,    top, within this tile
  int           ilolft;         // index to longitude,   left, within this tile
  int           ilorgt;         // index to longitude,  right, within this tile
  Point         ip;             // index of elevation point
  int           labote;         // effective index of bottom latitude, this tile
  int           latope;         // effective index of top    latitude, this tile
  int           lolfte;         // effective index of left  longitude, this tile
  int           lorgte;         // effective index of right longitude, this tile
  int           lontile;        // number of tile data records in longitude
  char          sd[2700];       // string for data
  char          sl[81];         // string for log file messages
int dx;

//  Determine which data records are to be read
  lontile = LonTile(latile) + 1;
  labote = (latile - glata) / glatd;
  latope = labote + LATTILE + 1;
  ilabot = max(0, labote);
  ilatop = min(ilabot + LATTILE + 1, glatn);
  lolfte = (lotile - glona) / glond;
  lorgte = lolfte + lontile;
  ilolft = max(0, lolfte);
  ilorgt = min(ilolft + lontile, glonn);

  if(gc[0] == 0.0)
//  Open the DEM file
  {
    if((fd = OpenDem(latile / 3600, lotile / 3600)) == NULL)
//  There is no data for this area; zero the elevations
    {
      for(ila = ilabot; ila < ilatop; ila++)
        for(ip = Index(ilolft, ila); ip < Index(ilorgt, ila); ip++)
          gel[ip] = 0;
      sprintf(sl, "****  No such file\n");
      WriteLog(sl);
      sprintf(sl, "  Zero elevations for indices %6d to %6d, %6d to %6d\n",
              ilabot, ilatop - 1, ilolft, ilorgt - 1);
      WriteLog(sl);
      return(-1);
    }
    else
    {
if ( !TDPS_flag )    
//  Read the User Header Label (bytes 1 to 80)
      fread(sd, 1,   80, fd);
else
      fread(sd,1,2402,fd);  // read and discard label information

//  Write the file's latitude and longitude to the log
if ( !TDPS_flag )
{
      sprintf(sl, "  %c%c%c %c%c%c%c file opened\n",
              sd[13], sd[14], sd[19], sd[4], sd[5], sd[6], sd[11]);
      WriteLog(sl);
}
      sprintf(sl, "  Set elevations for indices %6d to %6d, %6d to %6d\n",
              ilabot, ilatop - 1, ilolft, ilorgt - 1);
      WriteLog(sl);

if ( !TDPS_flag )    
{
//  Read and discard the Data Set Identification Record (bytes 81 to 728)
      fread(sd, 1,  648, fd);

//  Read and discard the Accuracy Record (bytes 729 to 3428)
      fread(sd, 1, 2700, fd);
//  Read the Data Records
      for(ilo = lolfte; ilo < lorgte; ilo++)
      {
//  Read and discard the record header
        fread(ida, 1,    8, fd);

//  Read the elevation data
        fread(ida, 1, 2402, fd);

        if(ilo >= ilolft && ilo < ilorgt)
//  Insert points in array gel
        {                                  // data goes S-N and W-E
          for (ila = ilabot; ila < ilatop; ila++)
            gel[Index(ilo, ila)] = Decode(ida[ila - labote]);
        }
//  Read and discard the record checksum
        fread(ida, 1,    4, fd);
      }
dx = LATTILE / LonTile(latile);
if ( dx > 1 )                     // expand data to fill in missing area
{
for ( ilo=lorgte-1 ; ilo > lolfte ; ilo-- )
{
  if( ilo >= ilolft && ilo < ilorgt )
  {
    for ( ila=ilabot ; ila < ilatop ; ila++ )
    {
      int elft,ergt,i,x;

      ergt = gel[Index(ilo, ila)];
      elft = gel[Index(ilo-1, ila)];
      x    = lolfte + (dx*(ilo - lolfte));
      for ( i=0 ; i < dx ; i++ )
      {
        gel[Index(x-i, ila)] = (((dx-i)*ergt) + (i*elft)) / dx;
      }
    }
  }
}

}
    }
    else                      // TDPS data
    {                         // data goes W-E and N-S
      for (ila = latope-1 ; ila >= labote ; ila--)
      {
        fread(ida, 1, 2402, fd);      //  Read the elevation data

        if(ila >= ilabot && ila < ilatop)
        {
          for(ilo = ilolft; ilo < ilorgt; ilo++)
            gel[Index(ilo, ila)] = Decode(ida[ilo - lolfte]);
        }
      }
    }

//  Close the DEM file
      fclose(fd);
  }
  }
  else
//  Create artificial elevation data
  {
    for(ilo = lolfte; ilo < lorgte; ilo++)
    {
      for(ila = labote; ila < latope; ila++)
      {
        el = gc[1] + (gc[2] + gc[4] * ilo) * ilo
           + (gc[3] + gc[5] * ila) * ila + gc[6] * ilo * ila;
        el = (el < gelmax) ? el : gelmax;
        el = (el > gelmin) ? el : gelmin;
        if((ilo >= ilolft)&&(ilo < ilorgt)&&(ila >= ilabot)&&(ila < ilatop))
          gel[Index(ilo, ila)] = el;
      }
    }
  }
  return(1);
}

/***********************************************************************
*   ReadDem: This routine reads the Digital Elevation Map data
*   into an array which will be available to all routines in the
*   Line of Sight program.
*   Returns the number of elevations that were read.
*/

int ReadDem()
{
  FILE          *fn;            // FILE pointer to open input file
  extern int    gbigend;        // bigendian = 1, little-endian = 0
  extern double gc[7];          // coefficients in elevation equation
  extern char   gDemDir[80];    // global DEM directory name
  extern double gearthr;        // radius of the Earth (meters)
  extern Elev   *gel;           // elevation (meters)
  extern int    gelmax;         // maximum allowed elevation
  extern int    gelmin;         // minimum allowed elevation
  extern Angle  glata;          // latitude,  absolute (arc seconds)
  extern Angle  glona;          // longitude, absolute (arc seconds)
  extern Angle  glatd;          // latitude,  delta    (arc seconds)
  extern Angle  glond;          // longitude, delta    (arc seconds)
  extern int    glatn;          // latitude,  number of points
  extern int    glonn;          // longitude, number of points
  extern Angle  gLatBot;        // latitude, bottom edge, playbox (arc seconds)
  extern Angle  gLatTop;        // latitude,    top edge, playbox (arc seconds)
  extern Angle  gLonLft;        // longitude,  left edge, playbox (arc seconds)
  extern Angle  gLonRgt;        // longitude, right edge, playbox (arc seconds)
  extern Point  gndem;          // number of points in DEM array
  short int     ifend = 0x0100; // variable to determine endian
  char          invar[12];      // name of input data item
  int           latile;         // tile bottom latitude (arc seconds)
  char          line[81];       // one line from input file
  int           lotile;         // tile left longitude (arc seconds)
  int           LatBot;         // latitude, bottom edge, tile (arc seconds)
  int           LatTop;         // latitude,    top edge, tile (arc seconds)
  int           LonLft;         // longitude,  left edge, tile (arc seconds)
  int           LonRgt;         // longitude, right edge, tile (arc seconds)
  char          sl[81];         // string for log file messages

  gc[0] = 0.0;
//  Determine endian
  if(*((char *) &ifend))
  {
    gbigend = 1;
    sprintf(sl, "ReadDem:    This is a bigendian machine\n");
    WriteLog(sl);
  }

  else
  {
    gbigend = 0;
    sprintf(sl, "ReadDem:    This is a little-endian machine\n");
    WriteLog(sl);
  }

//  Read the input file
  if((fn = fopen(NAMEINPUT, "r")) == NULL)
  {
    printf (    "Cannot open input file                  %s\n", NAMEINPUT);
    sprintf(sl, "Cannot open input file                  %s\n", NAMEINPUT);
    WriteLog(sl);
    exit(1);
  }
  else
  {
    while(fgets(line, 81, fn) != NULL)
    {
      if(line[0] != '!' && line[0] != '#' && line[0] != '\n')
//  Ignore comment lines
      {
        sscanf(line, "%s", invar);
        if(0 == strcmp(invar, "LatBot"))
          gLatBot = InputAngle(line);
        else if(0 == strcmp(invar, "LonLft"))
          gLonLft = InputAngle(line);
        else if(0 == strcmp(invar, "LatTop"))
          gLatTop = InputAngle(line);
        else if(0 == strcmp(invar, "LonRgt"))
          gLonRgt = InputAngle(line);
        else if(0 == strcmp(invar, "DemDir"))
          sscanf(line, "%*s%s", gDemDir);
        else if(0 == strcmp(invar, "EarthRad"))
          sscanf(line, "%*s%lf", &gearthr);
        else if(0 == strcmp(invar, "Coe"))
        {
          sscanf(line, "%*s%lf %lf %lf %lf %lf %lf",
            &gc[1], &gc[2], &gc[3], &gc[4], &gc[5], &gc[6]);
          gc[0] = 1.0;
        }
        else if(0 == strcmp(invar, "ElevLims"))
          sscanf(line, "%*s%d %d", &gelmax, &gelmin);
      }
    }
    fclose(fn);
  }
  sprintf(sl, "  Playbox latitude bottom (arcseconds)  %8d\n", gLatBot);
  WriteLog(sl);
  sprintf(sl, "  Playbox longitude left  (arcseconds)  %8d\n", gLonLft);
  WriteLog(sl);
  sprintf(sl, "  Playbox latitude top    (arcseconds)  %8d\n", gLatTop);
  WriteLog(sl);
  sprintf(sl, "  Playbox longitude right (arcseconds)  %8d\n", gLonRgt);
  WriteLog(sl);
  sprintf(sl, "  TIN directory:    %s\n", gDemDir);
  WriteLog(sl);
  if((gLatTop - gLatBot) < 0)
  {
    printf (    "Playbox latitude range is negative\n");
    sprintf(sl, "Playbox latitude range is negative\n");
    WriteLog(sl);
    exit(2);
  }
//  Allow for playboxes which include longitude 180
  if(gLonRgt < gLonLft)
    gLonRgt += 1296000;

//  Calculate the range of latitudes and longitudes of the tiles needed
  LatBot = lower((double)gLatBot);
  LatTop = upper((double)gLatTop);

//  Allow for "zero" height playbox
  if(LatTop - LatBot == 0)
    LatTop += 3600;
  LonLft = lower((double)gLonLft);
  LonRgt = upper((double)gLonRgt);

//  Allow for "zero" width playbox
  if(LonRgt - LonLft == 0)
    LonRgt += 3600;
  sprintf(sl, "   LatBot = %6d   LatTop = %6d   LonLft = %6d   LonRgt = %6d\n",
          LatBot, LatTop, LonLft, LonRgt);
  WriteLog(sl);

//  Allocate memory for the array of elevation points
  glata = gLatBot;
  glona = gLonLft;
  glatd = 3600 / LATTILE;
  glond = 3600 / LATTILE;                        // LonTile(LatBot);
  glatn = (gLatTop - gLatBot) / glatd + 1;
  glonn = (gLonRgt - gLonLft) / glond + 1;
  gndem = glatn * glonn;
  if(((float)glatn * (float)glonn) > MAXDEMN)
//  if(gndem > MAXDEMN)
  {
    printf (    "Requested playbox size (%lld DEM points) > capacity (%lld)\n",
           gndem, MAXDEMN);
    sprintf(sl, "Requested playbox size (%lld DEM points) > capacity (%lld)\n",
           gndem, MAXDEMN);
    WriteLog(sl);
    exit(2);
  }
  if((gel = (Elev *)malloc(gndem * sizeof(Elev))) == NULL)
  {
    printf (    "Insufficient memory to hold DEM elevations\n");
    sprintf(sl, "Insufficient memory to hold DEM elevations\n");
    WriteLog(sl);
    exit(2);
  }
//  Read DEM files from southwest to northeast
  for(latile = LatBot; latile < LatTop; latile += 3600)
  {
    for(lotile = LonLft; lotile < LonRgt; lotile += 3600)
    {
      sprintf(sl, "  Opening tile with bottom lat =  %6d and left lon =  %6d\n",
              latile, lotile);
      WriteLog(sl);
      ReadFile(latile, lotile);
    }
  }
  return(gndem);
}
