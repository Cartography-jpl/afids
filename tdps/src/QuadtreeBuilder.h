/*< QuadtreeBuilder.h >******************************************************
 * This file containes the definitions of the basic structures 
 * and routines used build quadtrees
 *
 * Written by Joseph E. Gonzalez 
 * email: josephg@caltech.edu
 *
 */

#include "Quadtree.h"
#ifndef QUADTREE_BUILDER
#define QUADTREE_BUILDER


/***************************************************************************
 *                              < Structures >                             *
 ***************************************************************************/

/***************************************************************************
 * This structure defines a DEM
 */
typedef struct {
   // The finest level resolution of a location measured in
   // ___________1__________ seconds
   //  2^locationResolution
   int locationResolution;
   
   // The defining dimensions of the DEM
   Angle leftLongitude, rightLongitude, topLatitude, bottomLatitude;

   // Angle between latitudes an longitudes (rows and colums) of DEM posts
   Angle deltaLat, deltaLon;
   // Number of rows and columns of data
   int rows, cols;
   // Stores the actual elevation data
   Elevation *data;
} DEM, *DEMPtr;




/***************************************************************************
 *                               < Routines >                              *
 ***************************************************************************/

/***************************< loadDEM Definition >**************************
 * Arguments:
 *    - none
 * Return: 
 *    - A pointer to a DEM data structure storing the elevation data  
 * Details:
 *    This routine should only be expected to run on the supercomputer
 *    and may use as much memory as it needs.
 */
DEMPtr loadDEM();



/***********************< buildQuadtree Definition >************************
 * Arguments:
 *    - DEMPtr dem -> This is a pointer to the dem data structure the 
 *          data in this structure will be reassigned to the 
 *          Quadtree so it should not be freed
 * Return: 
 *    - QuadtreePtr -> A pointer to a fully constructed Quadtree  
 * Details:
 *    This routine should only be expected to run on the supercomputer
 *    and may use as much memory as it needs.
 */
QuadtreePtr buildQuadtree(DEMPtr dem);



/***********************< getValueFromDEM Defintion >***********************
 * Arguments:
 *    - DEMPtr dem -> This is a pointer to the dem data structure the 
 *    - row -> index of the row in the DEM
 *    - column -> index of the col in the DEM
 * Return: 
 *    - Elevation -> The Elevation of the DEM post at the given row 
 *       column index
 * Details:
 *    none
 */
Elevation getValueFromDEM(DEMPtr dem, int row, int column);

/* Eventually the following routines will need to be added
 *    Compression
 *       either as part of buildQuadtree or as a seperate
 *       routine a compression algorithm needs to be developed
 *       to implement deltas, 0 value =approximately= 0 bits
 *       and possibly modulo calculus.
 */
#endif


