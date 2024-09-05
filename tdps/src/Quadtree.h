/*< Quadtree.h >*************************************************************
 * This file containes the definitions of the basic structures 
 * and routines used in the quadtree algorithm
 *
 * Written by Joseph E. Gonzalez 
 * email: josephg@caltech.edu
 *
 */

#include "DataSource.h" // DataSource Structure
#include "EndeanTool.h"

#ifndef QUADTREE
#define QUADTREE


// a boolean since c does not have booleans
typedef enum booleanTag {FALSE, TRUE} boolean;

// Define elevation as type value 
typedef Value Elevation; 

// Define lat/lon angle type. To reduce the complexity of the intersection
//    mathematics this will be measured in units of
//    seconds / (2^location resolution)
typedef int Angle;       


/***************************************************************************
 *                              < Structures >                             *
 ***************************************************************************/

/***************************************************************************
 * This structure defines a Quadtree. However all the elevation
 * data in the quadtree is encapsulated in the DataSourcePtr data
 */
typedef struct {
   // The locationResolution field determines the finest level resolution 
   // of a location measured in ___________1__________ seconds
   //                            2^locationResolution
   int locationResolution;
   
   /* The following fields define the dimensions of the dem in the
    * following manner:
    *
    *    ------< topLatitude >-------
    *    |                          |
    *    |                          |
    *    |                          |
    *    |                          |
    *    |                          |
    *< leftLongitude >          < rightLongitude >
    *    |                          |
    *    |                          |
    *    |                          |
    *    |                          |
    *    |                          |
    *    -----< bottomLatitude >-----
    */
   Angle leftLongitude, rightLongitude, topLatitude, bottomLatitude;
   
   // Stores the spacing between rows and columns of the DEM
   //    deltaRow = deltaLatitude
   //    deltaCol = deltaLongitude
   Angle deltaLat, deltaLon;

   // Stores the Actual tree data.  This is an ADT allowing for the future
   //    development of memory storage techniques 
   DataSourcePtr data;
} Quadtree, *QuadtreePtr;



/***************************************************************************
 * This structure defines a line segment between point 1 and point 2.
 * For purposes of intersection calculations a line segment also has a
 * vector direction that points from point 1 to point 2 and a curvature
 * coefficient. 
 */
typedef struct {
   Angle lat1, lon1; // Latitude and Longitude of Point 1
   int elev1; // Elevation of Point 1 relative to the geiod

   Angle lat2, lon2; // Latitude and Longitude of Point 2
   int elev2; // Elevation of Point 2 relative to the geiod 

   // vector pointing from point 1 to point 2  = (p2 - p1)
   Angle dirLat, dirLon; 
   int dirElev;

   // Line curvature factor
   double lineCurve; 
} LineSegment, *LineSegmentPtr;


/***************************************************************************
 *                               < Routines >                              *
 ***************************************************************************/

/*************************< getElevation Definition >***********************
 * Arguments:
 *    - QuadtreePtr tree -> The pointer to a tree structure
 *    - Angle lat -> The latitude of the point
 *    - Angle lon -> The longitude of a point
 * Return: 
 *    - Elevation -> The height of the terrain measured from the geoid at 
 *       the given point.  
 * Details:
 *    This routine interpolates between DEM posts by constructing
 *    a triangle connecting the three nearest DEM posts. 
 */
Elevation getElevation(QuadtreePtr tree, Angle lat, Angle lon);



/**********************< isLOSObstructed Definition >***********************
 * Arguments:
 *    - QuadtreePtr tree -> The pointer to a tree structure
 *    - Angle lat1 -> The latitude of the first point
 *    - Angle lon1 -> The longitude of the first point
 *    - Elevation height1 -> The height of the first point measured from
 *       the terrain
 *    - Angle lat2 -> The latitude of the second point
 *    - Angle lon2 -> The longitude of the second point
 *    - Elevation height2 -> The height of the second point measured from 
 *       the terrain
 * Return: 
 *    - bool -> Whether line-of-sight is obstructed  
 * Details:
 *    This routine interpolates between DEM posts by constructing
 *    triangles connecting the three nearest DEM posts.  
 *    This routine is at the core of the quadtree structures
 */
boolean isLOSObstructed(QuadtreePtr tree, 
                        Angle lat1, Angle lon1, Elevation height1,
                        Angle lat2, Angle lon2, Elevation height2);



/*************************< intersects Definition >*************************
 * Arguments:
 *    - LineSegmentPtr line: The pointer to the line segment
 *    - Angle planeLat1: Angle defining the latitude plane of the box 
 *       which is closest to the origin
 *    - Angle planeLat2: Angle defining the latitude plane of the box
 *       which is furthest from the origin
 *    - Angle planeLon1: Angle defining the longitude plane of the box 
 *       which is closest to the origin
 *    - Angle planeLon2: Angle defining the longitude plane of the box
 *       which is furthest from the origin
 *    - Elevation planeZ: The elevation of the plane defining the top
 *       of the box.
 * Return: 
 *    - boolean: If LOS defined by the line segment intersects the box 
 *                defined by the other arguments
 * Details:
 *    This routine is used to evaluate whether the line segment
 *    intersects nodes defined as bounding boxes whose height is the 
 *    maximum height of the region they contain.  Furthermore, the
 *    bounding box contains all the points along the linear interpolation
 *    between the DEM posts in the interior region.
 */

boolean intersects(LineSegmentPtr line, Angle planeLat1, Angle planeLat2, 
                   Angle planeLon1, Angle planeLon2, int planeZ);



/************************< intersectsLeaf Definition >**********************
 * Arguments:
 *    - LineSegmentPtr line: The pointer to the line segment
 *    - Angle planeLat1: Angle defining the latitude plane of the box 
 *       which is closest to the origin
 *    - Angle planeLat2: Angle defining the latitude plane of the box
 *       which is furthest from the origin
 *    - Angle planeLon1: Angle defining the longitude plane of the box 
 *       which is closest to the origin
 *    - Angle planeLon2: Angle defining the longitude plane of the box
 *       which is furthest from the origin
 *    - Elevation z1: The height of the DEM Post at the intersection of 
 *       planeLon1 and planeLat1
 *    - Elevation z2: The height of the DEM Post at the intersection of 
 *       planeLat1 and planeLon2
 *    - Elevation z3: The height of the DEM Post at the intersection of 
 *       planeLat2 and planeLon1
 *    - Elevation z4: The height of the DEM Post at the intersection of 
 *       planeLat2 and planeLon2
 *
 *    Stated Graphically:
 *       z1 lat1 lat1 lat1 lat1 z2  -----> Positive X direction
 *       lon1d1                 lon2
 *       lon1   d1              lon2
 *       lon1      d1           lon2
 *       lon1        d1         lon2
 *       lon1          d1       lon2
 *       lon1            d1     lon2
 *       lon1              d1   lon2
 *       lon1                d1 lon2
 *       z3 lat2 lat2 lat2 lat2 z4
 *
 *       |
 *       |
 *       V Positive Y Direction
 *
 * Return: 
 *    - boolean: If the line of sight defined by the line segment
 *         intersects the box defined by the other arguments
 * Details:
 *    This routine is used to evaluate whether the line segment
 *    intersects leaves defined as two triangular planes and the region
 *    of space directly below them.  This assumes a linear interpolation
 *    between DEM posts.  The diagonal is drawn as seen above.
 */
boolean intersectsLeaf(LineSegmentPtr line, 
                       Angle planeLat1, Angle planeLat2, 
                       Angle planeLon1, Angle planeLon2, 
                       int z1, int z2, int z3, int z4);


/**********************< openQuadtree Definition >**************************
 * Arguments:
 *    - FILE *file: The pointer to a file stream.
 *    - Endean fileEndean: The endean of the file 
 * Return: 
 *    - QuadtreePtr: the pointer to an initialized Quadtree
 * Details:
 *    This will be called to load an existing tree from a file.  It should
 *    be expected that a large ammount of memory will be malloced inside
 *    of this routine. 
 */
QuadtreePtr loadQuadtree(FILE *file, Endean fileEndean);



/**********************< saveQuadtree Definition >**************************
 * Arguments:
 *    - FILE *file: The pointer to a file stream.
 *    - QuadtreePtr: the pointer to an initialized Quadtree
 *    - Endean fileEndean: The endean of the file
 * Return: 
 *    - void:
 * Details:
 *    This will be called to dump a an existing tree to a file.  
 */
void saveQuadtree(FILE *file, QuadtreePtr tree, Endean fileEndian);


/***********************< displayTree Defintion >***************************
 * Arguments:
 *    - QuadtreePtr tree -> The pointer to a tree structure
 * Return: 
 *    - void
 * Effect:
 *    - Displays on the terminal the tree data in text form for the entire
 *       tree including DEM
 * Details:
 *    Because this routine displays everything it is recommended that
 *    it not be used on a full sized tree unless the output is sent to
 *    a file.
 */
void displayTree(QuadtreePtr tree);


/*******************< displayTreeHeader Defintion >*************************
 * Arguments:
 *    - QuadtreePtr tree -> The pointer to a tree structure
 * Return: 
 *    - void
 * Effect:
 *    - Displays the tree description information
 */
void displayTreeHeader(QuadtreePtr tree);


#endif

