/*< Quadtree.c >*************************************************************
 * This file contains the implementation of the functions defined in the
 * quadtree header file
 *
 * Written by Joseph E. Gonzalez 
 * email: josephg@caltech.edu
 *
 */

#include <stdio.h>
#include <math.h>
//#include <assert.h>
#include <stdlib.h>
#include "Quadtree.h"
#include "error_codes.h"


// Space Available for plain text at the top of a quadtree file
const int HEADER_SIZE = 12040; // 10kb

const double PI = 3.1415926535898; // Define PI

const double rfac = (3.1415926535898 / (3600 * 180));

const int EARTH_RADIUS = 6367000; // meters

const double ATM_REFRACT_FACTOR = 0.8279; // pure number

const double reatm = (6367000 / 0.8279);

boolean line_out_of_bounds(QuadtreePtr tree, Angle lat1, Angle lon1,
                                             Angle lat2, Angle lon2);
boolean no_los(QuadtreePtr tree, Angle lat1, Angle lon1, Elevation height1,
                                 Angle lat2, Angle lon2, Elevation height2);
 
 
/***************************************************************************
 *                      < Utility Functions >                              *
 ***************************************************************************/

boolean isBetween(double x, Angle a, Angle b){
   return ((a <= x) && (x < b))? TRUE : FALSE; 
} // isBetween

int applySign(int a, int b) { return (a >= 0)? b : -b; }

Angle absAngle(Angle a) { return (a >= 0)? a : -a; }

/***************************************************************************
 *                    < Internal Function Definitions >                    *
 ***************************************************************************/

/**********************<recIsLOSObstructed Definition >*********************
 * Arguments:
 *    - QuadtreePtr tree: The pointer to a tree structure
 *    - LineSegmentPtr line: The pointer to the line segment
 *    - level: The current level in the tree 
 *    - minRow: minimum row the of the DEM region being examined
 *    - maxRow: maximum row of the DEM region being examined
 *    - minCol: mininum column of the DEM region being examined
 *    - maxCol: maximum column of the DEM region being examined
 * Return: 
 *    - boolean: true if line of sight is obstructed false if line of sight
 *                is unobstructed
 * Details:
 *    This routine is the recursive routine which actually evaluates LOS
 *      it is called by the header-defined routine which first collects 
 *      the information needed to run this routine and then invokes this 
 *      routine.
 */
boolean recIsLOSObstructed(QuadtreePtr tree, LineSegmentPtr line, 
                           int level, 
                           int minRow, int maxRow, 
                           int minCol, int maxCol,
                           int indexRow, int indexCol);





/***************************************************************************
 *                        < Method Implementations >                       *
 ***************************************************************************/
 


/**********************< loadQuadtree Implementation >**********************
 * Already Defined in Header
 *    Implementation Comments:
 */
QuadtreePtr loadQuadtree(FILE *file, Endean fileEndean) {

   QuadtreePtr tree = (QuadtreePtr)malloc(sizeof(Quadtree));


   fseek(file, HEADER_SIZE, SEEK_SET);

   fread(&(tree->locationResolution), sizeof(int), 1, file);
   fread(&(tree->topLatitude), sizeof(Angle), 1, file);
   fread(&(tree->bottomLatitude), sizeof(Angle), 1, file);
   fread(&(tree->leftLongitude), sizeof(Angle), 1, file);
   fread(&(tree->rightLongitude), sizeof(Angle), 1, file);
   fread(&(tree->deltaLat), sizeof(Angle), 1, file);
   fread(&(tree->deltaLon), sizeof(Angle), 1, file);
   
   toNativeEndean(&(tree->locationResolution), fileEndean, sizeof(Angle), 1);
   toNativeEndean(&(tree->topLatitude), fileEndean, sizeof(Angle), 1);
   toNativeEndean(&(tree->bottomLatitude), fileEndean, sizeof(Angle), 1);
   toNativeEndean(&(tree->leftLongitude), fileEndean, sizeof(Angle), 1);
   toNativeEndean(&(tree->rightLongitude), fileEndean, sizeof(Angle), 1);

   toNativeEndean(&(tree->deltaLat), fileEndean, sizeof(Angle), 1);
   toNativeEndean(&(tree->deltaLon), fileEndean, sizeof(Angle), 1);

   tree->data = loadDataSource(file, fileEndean);

   return tree;
} // End of loadQuadtree




/**********************< dumpQuadtree Implementation >**********************
 * Already Defined in Header
 *    Implementation Comments:
 */
void saveQuadtree(FILE *file, QuadtreePtr tree, Endean fileEndean){
   printf("\n\nWriting a Quadtree out to Disk\n\n");
   
   printf("Writing Quadtree Header Information\n");
   // Dump text information about the tree to the file
   fprintf(file, "******************************************\n");
   fprintf(file, "*           Quadtree Data File           *\n");
   fprintf(file, "******************************************\n\n");

   fprintf(file, "This file was generated for the Corps Battle Simultion\n");
   fprintf(file, "and contains the elevation data for the quadtree\n");
   fprintf(file, "algorithm\n\n");
   
   fprintf(file, "The binary information stored in this file is in %s.\n\n", 
      ((fileEndean == 1)? "Big Endean" : "Little Endean"));

   fprintf(file, "Location Resolution:\t%f seconds\n",
      (1.0 / (1 <<tree->locationResolution)));
   
   fprintf(file, "The location of the upper left corner of the map is:\n");
   
   fprintf(file, "\t\t-> Latitude:\t%f seconds\n", 
      (double)(tree->topLatitude) / (1 << tree->locationResolution));
   
   fprintf(file, "\t\t-> Longitude:\t%f seconds\n", 
      (double)(tree->leftLongitude) / (1 << tree->locationResolution));

   fprintf(file,"\nThe Location of the lower right corner of the map is:\n");
   
   fprintf(file, "\t\t-> Latitude:\t%f seconds\n", 
     (double)(tree->bottomLatitude) / (1 << tree->locationResolution));
   
   fprintf(file, "\t\t-> Longitude:\t%f seconds\n", 
      (double)(tree->rightLongitude) / (1 << tree->locationResolution));
   
   fprintf(file, "The spacing between DEM posts:\n");
   
   fprintf(file, "\t\t-> Delta Latitude:\t%f seconds\n", 
      (double)tree->deltaLat
      / (1 <<tree->locationResolution));

   fprintf(file, "\t\t-> Delta Longitude:\t%f seconds\n", 
      (double)tree->deltaLon
      / (1 <<tree->locationResolution));
   
   // Dump header information to screen
   writeDataSourceHeader(file, tree->data);

   printf("Writing Quadtree binary information\n");

   // Seek to the beginning of the binary data
   fseek(file, HEADER_SIZE, SEEK_SET);

   // Begin Writing binary information describing the tree
   fwrite(toNativeEndean(&(tree->locationResolution), fileEndean,
      sizeof(int), 1), sizeof(int), 1, file);

   fwrite(toNativeEndean(&(tree->topLatitude), fileEndean, sizeof(Angle), 1), 
      sizeof(Angle), 1, file);
   fwrite(toNativeEndean(&(tree->bottomLatitude), fileEndean, sizeof(Angle), 1), 
      sizeof(Angle), 1, file);
   
   fwrite(toNativeEndean(&(tree->leftLongitude), fileEndean, sizeof(Angle), 1), 
      sizeof(Angle), 1, file);
   fwrite(toNativeEndean(&(tree->rightLongitude), fileEndean, sizeof(Angle), 1), 
      sizeof(Angle), 1, file);
   
   fwrite(toNativeEndean(&(tree->deltaLat), fileEndean, sizeof(Angle), 1), 
      sizeof(Angle), 1, file);

   fwrite(toNativeEndean(&(tree->deltaLon), fileEndean, sizeof(Angle), 1), 
      sizeof(Angle), 1, file);

   saveDataSource(file,tree->data, fileEndean);
   
   printf("Finished Writing Quadtree File");

} // End of dumpQuadtree




/**********************< getElevation Implementation >*********************
 * Already Defined in Header
 *    Implementation Comments:
 *     - The interpolation mathematics will be detailed in a paper soon to
 *       be released
 *     - This implementation was tested and produced the correct results
 *       using rectangular coordinates.  However it has since been changed
 *       to introduce polar addressing.
 */ 
//=========================================================================
Elevation getElevation(QuadtreePtr tree, Angle lat, Angle lon) {
   Angle relLat, relLon; // The relative latitude and longitude
      // measured from the upper left corner of the DEM
   int indexRow, indexCol; // Stores the DEM row and col of the
      // DEM post nearest the (lat,lon) position and the upper
      // left corner of the DEM.  Moreover, indexRow and indexCol
      // are the row an column to the upper left of the given point.
   double tLon, tLat; // Used in the interpolation

Value v1,v2,v3;

int iscale,ideg,temp_relLon;
int temp_deltaLon, indexCol_1, indexCol_2, temp_indexCol;

   if ( !( (((lat < tree->bottomLatitude) && (lat > tree->topLatitude)) ||
      ((lat > tree->bottomLatitude) && (lat < tree->topLatitude))) &&
      (((lon < tree->leftLongitude) && (lon > tree->rightLongitude)) ||
      ((lon > tree->leftLongitude) && (lon < tree->rightLongitude))) ) )
   {
     return ( ERROR_ELEV_LATLON );
   }


//   relLat = absAngle(lat - tree->topLatitude);
   relLat = absAngle(lat - tree->bottomLatitude);
   relLon = absAngle(lon - tree->leftLongitude);

   // Compute the indexRow and indexCol
   indexRow = (int)floor((double) relLat / tree->deltaLat);
//   indexCol = (int)floor((double) relLon / tree->deltaLon);

                      // fix for variable data spacing
temp_indexCol = (int)floor((double) relLon / tree->deltaLon);
   
iscale   = 0;    //LonTile(tree->bottomLatitude / 32) / LonTile(lat / 32);
if ( iscale > 1 )
{
ideg          = relLon / 115200;         // 3600 sec * 32
temp_relLon   = ideg * 115200;
temp_deltaLon = iscale * tree->deltaLon;
indexCol_1    = (int)floor((double) temp_relLon / tree->deltaLon);
indexCol_2    = (int)floor((double) (relLon - temp_relLon) / temp_deltaLon);
indexCol      = indexCol_1 + indexCol_2;
}
else
{
   indexCol = temp_indexCol;
}

   /* Usefull Diagram
    *
    *    (indexRow, indexCol)
    *    V________________\ (indexRow, indexCol + 1)
    *    |\            |  /
    *    |  \    T2    |   relLat
    *    |    \        |
    *    |      \      |
    *    |  T1    \    |
    *    |          \  |
    *    |____________\|(indexRow + 1, indexCol + 1)
    *    |\(indexRow + 1, indexCol)  
    *    V 
    *     reLon
    *
    * Let Point pt = (relLat,relLon)
    * 
    * Equation of dividing line is
    * relLat - indexRow * deltaLat = 
    *    relLon - indexCol * deltaLon
    *
    * A point pt in T1 satisfies the equaiton
    *    relLat - indexRow * deltaLat > 
    *       relLon - indexCol * deltaLon
    *
    * A point pt in T2 satisfies the equation
    *    relLat - indexRow * deltaLat <= 
    *       relLon - indexCol * deltaLon
    *
    * Because height is a conservative function in 
    *    Euclidian space we need only to compute the 
    *    sum of the change in height along some path 
    *    from a known height to the height we desire.
    *
    *    Thus we will choose the two axes and compute 
    *    the rise in height on one axes and the rise 
    *    in height on the other and sum them
    */
   if((relLat - (indexRow * tree->deltaLat)) > 
      (relLon - (temp_indexCol * tree->deltaLon))) {
      // Assert pt is in T1 therefore determine the
      // height of T1 at pt
      // Measure from lower left corner
      tLon = (double)(relLon - (temp_indexCol * tree->deltaLon)
         ) / tree->deltaLon;
      tLat = (double)(((indexRow + 1) * tree->deltaLat) - relLat
         ) / tree->deltaLat;

      // Interpolate using the technique described above
if ( (v1 = getDEMValue(tree->data, indexRow + 1, indexCol)) <= ERROR_CODES )
  return ( v1 );
if ( (v2 = getDEMValue(tree->data, indexRow + 1, indexCol + 1)) <= ERROR_CODES )
  return ( v2 );
if ( (v3 = getDEMValue(tree->data, indexRow, indexCol)) <= ERROR_CODES )
  return ( v3 );
      return (short) ((
            (1.0 - tLon) * v1 
            +
            tLon * v2
         ) +  (
            (1.0 - tLat) * v1 
            + 
            tLat * v3
         ) - v1);
   } else {
      // Assert pt is in T2 therefore determine the
      // height of T2 at pt
      // Measure from upper right corner
      tLon = (float)(((temp_indexCol + 1) * tree->deltaLon) - relLon
         )/tree->deltaLon;
      tLat = (float)(relLat - (indexRow * tree->deltaLat)
         )/tree->deltaLat;

      // Interpolate using the technique described above
if ( (v1 = getDEMValue(tree->data, indexRow, indexCol + 1)) <= ERROR_CODES )
  return ( v1 );
if ( (v2 = getDEMValue(tree->data, indexRow + 1, indexCol + 1)) <= ERROR_CODES )
  return ( v2 );
if ( (v3 = getDEMValue(tree->data, indexRow, indexCol)) <= ERROR_CODES )
  return ( v3 );
      return (short) ((
            (1.0 - tLon) * v1 + 
            tLon * v3
         ) + (
            (1.0 - tLat) * v1 + 
            tLat * v2
         ) - v1);
   } // End of if in which triangle
} // End of getElevation





/*******************< isLOSObstructed Implementation >***********************
 * Already Defined in Header
 *    Implementation Comments:
 *     - THIS ROUTINE HAS NOT BEEN TESTED FOR ACCURACY
 *     - THIS ROUTINE DOES NOT KEEP TRACK OF THE CURATURE OF THE EARTH
 *          AND IT SHOULD.
 */
//===========================================================================
boolean isLOSObstructed(QuadtreePtr tree, 
                        Angle lat1, Angle lon1, Elevation height1, 
                        Angle lat2, Angle lon2, Elevation height2){
Elevation e1,e2;
   double dirLatRadians, dirLonRadians;
   double lat1Radians, lat2Radians, distance;

   LineSegment line; // In rectangular 

//   line.lat1 = absAngle(lat1 - tree->topLatitude);
//   line.lat2 = absAngle(lat2 - tree->topLatitude);
   line.lat1 = absAngle(lat1 - tree->bottomLatitude);
   line.lat2 = absAngle(lat2 - tree->bottomLatitude);
   
   line.lon1 = absAngle(lon1 - tree->leftLongitude);
   line.lon2 = absAngle(lon2 - tree->leftLongitude);
   
   // Set the values
e1 = getElevation(tree, lat1, lon1);
e2 = getElevation(tree, lat2, lon2);
if ( (e1 <= ERROR_CODES) || (e2 <= ERROR_CODES) )
  e1 = e2 = 0;
   line.elev1 = e1 + height1;// + 2;
   line.elev2 = e2 + height2;// + 2;
//if ( (line.elev1 <= ERROR_CODES) || (line.elev2 <= ERROR_CODES) )
//{
//  fprintf(stderr,"ERROR in LOS elevation\n");
//  return ( TRUE );               // NO LOS
//}

   // Computing the direction vector
   line.dirLat = line.lat2 - line.lat1;
   line.dirLon = line.lon2 - line.lon1;
   line.dirElev = line.elev2 - line.elev1;

   dirLatRadians = (((double)line.dirLat / (1 << tree->locationResolution))
      * (PI / (3600 * 180))); 
   dirLonRadians = (((double)line.dirLon / (1 << tree->locationResolution))
      * (PI / (3600 * 180)));

   lat1Radians = ((double) lat1 / (1 << tree->locationResolution)) * 
      (PI / (3600 * 180));
   lat2Radians = ((double) lat2 / (1 << tree->locationResolution)) *
      (PI / (3600 * 180));

   distance = EARTH_RADIUS * 2.0 * asin( sqrt( 
      sin(dirLatRadians / 2.0) * sin(dirLatRadians / 2.0) + 
      cos(lat1Radians) * cos(lat2Radians) *
      sin(dirLonRadians / 2.0) * sin(dirLonRadians / 2.0)));

   if ( line_out_of_bounds(tree,lat1,lon1,lat2,lon2) )
   {
     double h1 = height1;
     double h2 = height2;
     double x = sqrt((2.0*reatm*h1)+(h1*h1)) + sqrt((2.0*reatm*h2)+(h2*h2));
     return ( distance > x ); 
   }

   line.lineCurve = distance * distance  * ATM_REFRACT_FACTOR /
      (2.0 * EARTH_RADIUS);

   // Return the results (Note: 1 << n = 2^n)
   return recIsLOSObstructed(tree, &line, 0, 
      0, 1 << getMaxLevel(tree->data),
      0, 1 << getMaxLevel(tree->data),
      0, 0);
} // End of isLOSObstructed





/*******************< recIsLOSObstructed Implementation >*******************
 * Already Defined in at the beginning of this file
 *    Implementation Comments:
 *     - THIS ROUTINE HAS NOT BEEN TESTED FOR ACCURACY
 *     - THIS ROUTINE DOES NOT KEEP TRACK OF THE CURATURE OF THE EARTH
 *          AND IT SHOULD.
 */
//==========================================================================
boolean recIsLOSObstructed(QuadtreePtr tree, LineSegmentPtr line, 
                           int level,
                           int minRow, int maxRow,
                           int minCol, int maxCol,
                           int indexRow, int indexCol){
   
   int midRow, midCol;

   // Determine whether the current region is even in the DEM
   // if its not then abort without intersection
   if(indexRow >= getRowDim(tree->data, level) ||
      indexCol >= getColDim(tree->data, level))
      return FALSE;



   // Base Case: (Leaf)
   if(level == getMaxLevel(tree->data)) {
      // The first part of this test determines if the leaf
      // is entirely in the DEM and the second part determines if
      // the leaf intersects the line of sight

Value v1,v2,v3,v4;
v1 = getDEMValue(tree->data, minRow, minCol);
v2 = getDEMValue(tree->data, minRow, maxCol);
v3 = getDEMValue(tree->data, maxRow, minCol);
v4 = getDEMValue(tree->data, maxRow, maxCol);
if ( (v1 <= ERROR_CODES) || (v2 <= ERROR_CODES) || (v3 <= ERROR_CODES) ||
                                                         (v4 <= ERROR_CODES) )
  return ( FALSE );

      return ((maxRow < getDEMRowDim(tree->data)) &&
              (maxCol < getDEMColDim(tree->data)) &&
              intersectsLeaf(line,  
                  minRow * tree->deltaLat,
                  maxRow * tree->deltaLat, 
                  minCol * tree->deltaLon, 
                  maxCol * tree->deltaLon, 
                  v1,
                  v2,
                  v3,
                  v4))? TRUE : FALSE;
      // Otherwise this is a node so we check to see if the LOS
      // intersects the node
   } 
   else 
   {
     Value val = getValue(tree->data,level, indexRow, indexCol);
     if ( val <= ERROR_CODES )
     {
       fprintf(stderr,"ERROR in getValue (col/row)\n");
       return ( FALSE );
     }  
     if(intersects(line, 
                        minRow * tree->deltaLat,
                        maxRow * tree->deltaLat,
                        minCol * tree->deltaLon, 
                        maxCol * tree->deltaLon, 
                        val
                        + getEarthCurveOffset(tree->data,level) ) ) {
      // Assert that the LOS does in fact intersect the node

      // Compute the dividing rows and cols
      midRow = ((maxRow - minRow) >> 1) + minRow;
      midCol = ((maxCol - minCol) >> 1) + minCol;  

      // Check to see if it intersects any of the children
      // If it does then immediately return true otherwise continue
      // to check
      if(recIsLOSObstructed(tree, line, level + 1, minRow, midRow,
         minCol, midCol, indexRow << 1, indexCol << 1))
         return TRUE;

      if(recIsLOSObstructed(tree, line, level + 1, minRow, midRow, 
         midCol, maxCol, indexRow << 1, 1 + (indexCol << 1)))
         return TRUE;

      if(recIsLOSObstructed(tree, line, level + 1, midRow, maxRow,
         minCol, midCol, 1 + (indexRow << 1), indexCol << 1))
         return TRUE;

      if(recIsLOSObstructed(tree, line, level + 1, midRow, maxRow,
         midCol, maxCol, 1 + (indexRow << 1), 1 + (indexCol << 1)))
         return TRUE;
   } // End of else if
   }
   // If nothing resulted in an intersection terminate this path of
      // recursion without intersection
   return FALSE; 

} // end of recIsLOSObstructed




/************************< intersects Implementation >**********************
 * Already Defined in at the beginning of this file
 *    Implementation Comments:
 */
//==========================================================================
boolean intersects(LineSegmentPtr line, 
                   Angle planeLat1, Angle planeLat2, 
                   Angle planeLon1, Angle planeLon2, 
                   int planeZ) {
   double t;

   // Are either of the lines end points in the box.
   if((isBetween(line->lat1, planeLat1, planeLat2) && 
      isBetween(line->lon1, planeLon1, planeLon2) && 
      (line->elev1 <= planeZ)) || ( 
      isBetween(line->lat2, planeLat1, planeLat2 && 
      isBetween(line->lon2, planeLon1, planeLon2) && 
      (line->elev2 <= planeZ))))
      return TRUE;

   // Test Intersection with Lon planes
   if(line->dirLon == 0){
      if(!isBetween(line->lon1, planeLon1, planeLon2))
         return FALSE;
   } else {
      // planeLon1
      t = (double)(planeLon1 - line->lon1)/line->dirLon;
      if(isBetween(t, 0, 1) && 
         isBetween(t * line->dirLat + line->lat1, planeLat1, planeLat2) &&
         ((t * (line->dirElev - line->lineCurve) + line->elev1) 
         <= (planeZ - (t * t) * line->lineCurve)))
         return TRUE;
      
      // planeLon2
      t = (double)(planeLon2 - line->lon1)/line->dirLon;
      if(isBetween(t, 0, 1) && 
         isBetween(t * line->dirLat + line->lat1, planeLat1, planeLat2) &&
         ((t * (line->dirElev - line->lineCurve) + line->elev1) 
         <= (planeZ - (t * t) * line->lineCurve)))
         return TRUE;
   }

   // Test Intersection with Lat planes
   if(line->dirLat == 0){
      if(!isBetween(line->lat1, planeLat1, planeLat2))
         return FALSE;
   } else {
      // planeLat1
      t = (double)(planeLat1 - line->lat1)/line->dirLat;
      if(isBetween(t, 0, 1) && 
         isBetween(t * line->dirLon + line->lon1, planeLon1, planeLon2) &&
         ((t * (line->dirElev - line->lineCurve) + line->elev1) 
         <= (planeZ - (t * t) * line->lineCurve)))
         return TRUE;
      
      // planeLat2
      t = (double)(planeLat2 - line->lat1)/line->dirLat;
      if(isBetween(t, 0, 1) && 
         isBetween(t * line->dirLon + line->lon1, planeLon1, planeLon2) &&
         ((t * (line->dirElev - line->lineCurve) + line->elev1) 
         <= (planeZ - (t * t) * line->lineCurve)))
         return TRUE; 
   }

   return FALSE; 
} // end of intersects




/*********************< intersectsLeaf Implementation >*********************
 * Already Defined in at the beginning of this file
 *    Implementation Comments:
 */
//==========================================================================
boolean intersectsLeaf(LineSegmentPtr line, 
                       Angle planeLat1, Angle planeLat2, 
                       Angle planeLon1, Angle planeLon2, 
                       int z1, int z2, int z3, int z4){
   double t; // Parameter for Line
   // Test Intersection with Lon planes
   if(line->dirLon == 0){
      if(!isBetween(line->lon1, planeLon1, planeLon2))
         return FALSE;
   } else {
      // plon11
      t = (double)(planeLon1 - line->lon1)/line->dirLon;
      if(isBetween(t, 0, 1) && 
         isBetween(t * line->dirLat + line->lat1, planeLat1, planeLat2) &&
         ((t * (line->dirElev - line->lineCurve) + line->elev1) <= ((z3 - z1) * 
         (t * line->dirLat + line->lat1 - planeLat1) / 
         (planeLat2 - planeLat1) + z1 - (t * t) * line->lineCurve)))
         return TRUE;
      
      // plon2
      t = (double)(planeLon2 - line->lon1)/line->dirLon;
      if(isBetween(t, 0, 1) && 
         isBetween(t * line->dirLat + line->lat1, planeLat1, planeLat2) &&
         ((t * (line->dirElev - line->lineCurve) + line->elev1) <= ((z4 - z2) * 
         (t * line->dirLat + line->lat1 - planeLat1) / 
          (planeLat2 - planeLat1) + z2 - (t * t) * line->lineCurve)))
         return TRUE;
   }

   // Test Intersection with Lat planes
   if(line->dirLat == 0){
      if(!isBetween(line->lat1, planeLat1, planeLat2))
         return FALSE;
   } else {
      // planeLat1
      t = (double)(planeLat1 - line->lat1)/line->dirLat;
       if(isBetween(t, 0, 1) && 
         isBetween(t * line->dirLon + line->lon1, planeLon1, planeLon2) &&
         ((t * (line->dirElev - line->lineCurve) + line->elev1) <= ((z2 - z1) * 
         (t * line->dirLon + line->lon1 - planeLon1) / 
         (planeLon2 - planeLon1) + z1 - (t * t) * line->lineCurve)))
         return TRUE;
      
      // planeLat2
      t = (double)(planeLat2 - line->lat1) / line->dirLat;
      if(isBetween(t, 0, 1) && 
         isBetween(t * line->dirLon + line->lon1, planeLon1, planeLon2) &&
         ((t * (line->dirElev - line->lineCurve) + line->elev1) <= ((z4 - z3) * 
         (t * line->dirLon + line->lon1 - planeLon1) / 
         (planeLon2 - planeLon1) + z3 - (t * t) * line->lineCurve)))
         return TRUE;
   }
   
   // Diagonal plane
   // Y planes
   if((line->dirLon - line->dirLat) == 0){
         return FALSE;
   } else {
      // Diagonal
      t = (double)((planeLon1 - planeLat1) - (line->lon1 - line->lat1)) / 
         (line->dirLon - line->dirLat);
      if(isBetween(t, 0, 1) && 
         isBetween(t * line->dirLon + line->lon1, planeLon1, planeLon2) &&
         ((t * (line->dirElev - line->lineCurve) + line->elev1) <= ((z4 - z1) * 
         (t * line->dirLon + line->lon1 - planeLon1) / 
         (planeLon2 - planeLon1) + z1 - (t * t) * line->lineCurve)))
         return TRUE;
   }
   return FALSE;
} // end of intersectsLeaf





/************************< displayTree Implementation >*********************
 * Already Defined in at the beginning of this file
 *    Implementation Comments:
 */
//==========================================================================
void displayTree(QuadtreePtr tree) {
   int level, row, col;

   printf("Upper left (lat, lon) = (%d, %d)\n",
      tree->topLatitude, tree->rightLongitude);   


   for(level = 0; level < getMaxLevel(tree->data); level++){
      printf("\nLevel: %d\n", level);
      for(row = 0; row < getRowDim(tree->data, level); row++){
         if((row & 1) == 0) {
            for(col = 0; col < getColDim(tree->data, level); col++){
               if((col & 1) == 0)
                  printf("----");
               else
                  printf("----|");
            } // End of col
            printf("\n");
            
         }
         for(col = 0; col < getColDim(tree->data, level); col++){
            if((col & 1) == 0)
               printf("%4d", getValue(tree->data, level, row, col));
            else
               printf("%4d|", getValue(tree->data, level, row, col));
         } // End of col
         printf("\n");
      } // End of row
   } // End of for level

   printf("DEM Data\n ");
   for(col = 0; col < getDEMColDim(tree->data); col++){
      if((col & 1) == 0)
         printf("---|");
      else
         printf("----");
   } // End of col
   printf("\n");

   for(row = 0; row < getDEMRowDim(tree->data); row++){
      if((row & 1) == 0) {
         printf("-");
         for(col = 0; col < getDEMColDim(tree->data); col++){
            printf("%4d", getDEMValue(tree->data, row, col));
         } // End of col
         printf("----");
      } else {
         printf(" ");
         for(col = 0; col < getDEMColDim(tree->data); col++){
            printf("%4d", getDEMValue(tree->data, row, col));
         } // End of col
      }
      printf("\n\n");
   } // End of row

   printf(" ");
   for(col = 0; col < getDEMColDim(tree->data); col++){
      if((col & 1) == 0)
         printf("---|");
      else
         printf("----");
   } // End of col
   printf("\nDone!\n");
} // end of displayTree



/*******************< displayTreeHeader Implementation >********************
 * Already Defined in at the beginning of this file
 *    Implementation Comments:
 */
//==========================================================================
void displayTreeHeader(QuadtreePtr tree) {
   // Dump text information about the tree to the file
   printf("**********************************************\n");
   printf("*            Quadtree Description            *\n");
   printf("**********************************************\n\n");

   printf("This file was generated for the Corps Battle Simultion\n");
   printf("and contains the elevation data for the quadtree\n");
   printf("algorithm\n\n");

   printf("Location Resolution:\t%f seconds\n",
      (1.0 / (1 <<tree->locationResolution)));
   
   printf("The location of the upper left corner of the map is:\n");
    
   printf("\t\t-> Latitude:\t%f seconds\n", 
      (double)(tree->topLatitude) / (1 << tree->locationResolution));
   
   printf("\t\t-> Longitude:\t%f seconds\n", 
      (double)(tree->leftLongitude) / (1 << tree->locationResolution));

   printf("\nThe Location of the lower right corner of the map is:\n");
   
   printf("\t\t-> Latitude:\t%f seconds\n", 
     (double)(tree->bottomLatitude) / (1 << tree->locationResolution));
   
   printf("\t\t-> Longitude:\t%f seconds\n", 
      (double)(tree->rightLongitude) / (1 << tree->locationResolution));
   
   printf("The spacing between DEM posts:\n");
   
   printf("\t\t-> Delta Latitude:\t%f seconds\n", 
      (double)tree->deltaLat / (1 <<tree->locationResolution));

   printf("\t\t-> Delta Longitude:\t%f seconds\n", 
      (double)tree->deltaLon / (1 <<tree->locationResolution));

   displayDataSourceHeader(tree->data);
} // end of displayTreeHeader


/***********************************************************************
*   LonTile: gives the number of points in the longitudinal direction
*   per 1 degree tile.
*   This currently handles DTED1 files and will be modified at some
*   future date to handle DTED2 files as well.
*   Returns the number of points.
*/

int LonTile
(
  int           la              // latitude (arc seconds)
)
{
  int           lontile;        // number of points per 1 degree tile
  la /= 3600;
  if     (-51 < la && la < 50)
    lontile = 1200;
  else if(-71 < la && la < 70)
    lontile = 600;
  else if(-76 < la && la < 75)
    lontile = 400;
  else if(-81 < la && la < 80)
    lontile = 300;
  else
    lontile = 200;
  return(lontile);
}

boolean line_out_of_bounds(QuadtreePtr tree, Angle lat1, Angle lon1,
                                             Angle lat2, Angle lon2)
{
   if ( (lat1 < tree->bottomLatitude) || (lat1 > tree->topLatitude)    ||
        (lat2 < tree->bottomLatitude) || (lat2 > tree->topLatitude)    ||  
        (lon1 < tree->leftLongitude)  || (lon1 > tree->rightLongitude) ||
        (lon2 < tree->leftLongitude)  || (lon2 > tree->rightLongitude) )
     return ( TRUE );
   return ( FALSE );
 
}

boolean no_los(QuadtreePtr tree, Angle lat1, Angle lon1, Elevation height1,
                                 Angle lat2, Angle lon2, Elevation height2)
{
  static double c2,l,l1,l2,t,t1,t2,r1,r2,x,y;

  l1 = ((double) lat1 / (1 << tree->locationResolution)) * rfac;
 
  t1 = acos ( reatm / ((reatm+height1)*cos(l1)) );
  t2 = ((double) abs(lon1-lon2) / (1<<tree->locationResolution)); 
  t2 = t2 * rfac; 
  t2 = t2 - t1; 
  r2 = reatm / cos(t2);

  l2 = ((double) lat2 / (1 << tree->locationResolution)) * rfac;
  x  = (reatm + height2) * cos(l2);

  if ( x < r2 )
    return ( TRUE );
  return ( FALSE );
} 
