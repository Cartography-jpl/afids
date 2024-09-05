/*< QuadtreeBuilder.c >******************************************************
 * This file contains the implementation of the functions defined in the
 * quadtree builder header file
 *
 * Written by Joseph E. Gonzalez 
 * email: josephg@caltech.edu
 *
 */
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include "QuadtreeBuilder.h"
#include "DataSource.h"
#include "Quadtree.h"
#include <stdio.h>
#include <time.h>
#include "Dem.h"


const short MIN_SHORT = -32768;
extern double PI;
extern int EARTH_RADIUS;


/***************************************************************************
 *                      < Utility Functions >                              *
 ***************************************************************************/
// Computes the maximum of two numbers
Elevation maxElevation(Elevation a, Elevation b) { return (a > b)? a : b; }

double maxDouble(double a, double b) { return (a > b)? a : b; }

/***************************************************************************
 *                    < Internal Function Definitions >                    *
 ***************************************************************************/
/******************< recBuildQuadTree Definition >**************************
 * Arguments:
 *    - QuadtreePtr tree -> The pointer to a tree structure
 *    - LineSegmentPtr line -> The pointer to the line segment
 *    - level -> The current level in the tree 
 *    - minRow -> minimum row of the DEM region being examined
 *    - maxRow -> maximum row of the DEM region being examined
 *    - minCol -> mininum column of the DEM region being examined
 *    - maxCol -> maximum column of the DEM region being examined
 *    - indexRow -> the row index of the current entry
 *    - indexCol -> the col index of the current entry
 * Return: 
 *    - Elevation -> The height of the highest point in the region
 * Details:
 *    This routine is the recursive routine which actually builds the
 *    tree it is called by the buildTree routine defined in the header
 */
Elevation recBuildQuadtree(QuadtreePtr tree, int level, 
                           int minRow, int maxRow, 
                           int minCol, int maxCol, 
                           int indexRow, int indexCol);


/***************************************************************************
 *                        < Method Implementations >                       *
 ***************************************************************************/
/***************************< loadDEM Implementation >**********************
 * Arguments:
 *    None
 * Return: 
 *    - DEMPtr -> A pointer to a struct which stores the elevation data
 *       in the DEM.
 * Details:
 *   This routine currently genererates an imaginary terrain.  It should
 *    be replaced by Gary's code to read a real DEM.
 */
DEMPtr loadDEM() {
   // Declare the dem and start to initialize fields
   DEMPtr dem = (DEMPtr)malloc(sizeof(DEM));

   printf("Loading DEM\n");    

   ReadDem();
   
   dem->locationResolution = 5;
   dem->topLatitude = gLatTop * (1 << dem->locationResolution);
   dem->leftLongitude = gLonLft * (1 << dem->locationResolution);
   dem->bottomLatitude = gLatBot * (1 << dem->locationResolution);
   dem->rightLongitude = gLonRgt * (1 << dem->locationResolution);
   dem->deltaLat = glatd * (1 << dem->locationResolution);
   dem->deltaLon = glond * (1 << dem->locationResolution);
   dem->rows = glatn;
   dem->cols = glonn;
   dem->data = gel;



   printf("Finished Loading DEM\n");
   
   return dem;
} // End of loadDem


 
/********************< buildQuadtree Implementation >***********************
 * Already Defined in Header
 *    Implementation Comments:
 *     - None at this time
 */
QuadtreePtr buildQuadtree(DEMPtr dem) {
   int i; // temporary counter variable
   QuadtreePtr tree; // Defines a pointer to the tree struct
   DataSourcePtr dataSource;

   printf("Allocating Memory for Tree\n");

   tree = (QuadtreePtr) malloc(sizeof(Quadtree));
   dataSource = tree->data = 
      (DataSourcePtr) malloc(sizeof(DataSource));
   
   // This formula determines the depth of the tree.  
   // Because the tree divides in half each time, the dimensions
   // of the side must be of the form 2^n.
   // However, since the DEM is divided along dem posts it
   // must actually be of the form 2^n + 1.
   // Hence the depth n = log[(Max DEM Dim) - 1] / log[2].
   dataSource->maxLevel = (int) ceil((double)log(
      (double)maxDouble(dem->rows, dem->cols) - 1.0) / log(2.0));

   tree->locationResolution = dem->locationResolution;
   tree->topLatitude = dem->topLatitude;
   tree->bottomLatitude = dem->bottomLatitude;
   tree->leftLongitude = dem->leftLongitude;
   tree->rightLongitude = dem->rightLongitude;

   tree->deltaLat = dem->deltaLat;
   tree->deltaLon = dem->deltaLon;

   printf("Levels In Tree: %d levels\n", (dataSource->maxLevel + 1));
   
   // Allocate the memory for storing the dimension information (structs)
   // for each level of the tree
   dataSource->levelDim = (DimensionPtr) malloc(sizeof(Dimension) 
      * (dataSource->maxLevel + 1));

   dataSource->earthCurvatureOffset = (int*) malloc(sizeof(Dimension)
      * (dataSource->maxLevel + 1));

   // Allocate the memory for the array of pointers to each level 
   // in the tree
   dataSource->data = (Elevation**)malloc(sizeof(Elevation*) 
      * (dataSource->maxLevel + 1));

   // Point the last level to the DEM array
   dataSource->data[dataSource->maxLevel] = dem->data;
   
   // set the dimension of the last level (dem level)
   dataSource->levelDim[dataSource->maxLevel].rows = dem->rows;
   dataSource->levelDim[dataSource->maxLevel].cols = dem->cols;

   // compute the dimensions of each of the remaining levels
   // in the tree and allocate the necessary memory
   /* The math:
    *    #of squares in a given level of the tree is equal to the 
    *    ratio of the #dem posts to the size of the 2^n containing
    *    grid dem posts times the 2^n grid for the given level
    */
   for(i = 0; i < dataSource->maxLevel; i++) {
      dataSource->levelDim[i].rows = (int)ceil(((double)(dem->rows - 1)
         / (1 << dataSource->maxLevel)) * (1 << i));
      dataSource->levelDim[i].cols = (int)ceil(((double)(dem->cols - 1)
         / (1 << dataSource->maxLevel)) * (1 << i));

      dataSource->data[i] = (Elevation*)malloc(sizeof(Elevation)
         * dataSource->levelDim[i].rows * dataSource->levelDim[i].cols);
   } // end of for loop




   // Print the dimensions to the console for checking purposes
   for(i = 0; i <= dataSource->maxLevel; i++) {
      printf("Level: %d\t rows: %d\t columns: %d\n", i, 
         dataSource->levelDim[i].rows,
         dataSource->levelDim[i].cols);

      dataSource->earthCurvatureOffset[i] = (int)(2.0 * (double)EARTH_RADIUS * 
         sin(((double)dem->deltaLat / (double)(1 << tree->locationResolution)) 
            * (double)(1 << (dataSource->maxLevel - i)) * (PI / (3600.0 * 180.0)) 
            ) *
         sin(((double)dem->deltaLat / (double)(1 << tree->locationResolution)) 
            * (double)(1 << (dataSource->maxLevel - i)) * (PI / (3600.0 * 180.0)) 
            )
            );
   } // end of for loop

   printf("Recursively Building the Tree\n");

   // Recursively building the tree (Note 1<<n = 2^n)
   recBuildQuadtree(tree, 0, 
      0, 1 << dataSource->maxLevel,
      0, 1 << dataSource->maxLevel,
      0,0);

   // return the resulting tree
   printf("Finished Building the Tree\n");

   return tree;
} // End of buildTree



/******************< recBuildQuadTree Implementation >**********************
 * Already Defined in Header
 *    Implementation Comments:
 *     - None at this time
 */
// =========================================================================
Elevation recBuildQuadtree(QuadtreePtr tree, int level, 
                           int minRow, int maxRow, 
                           int minCol, int maxCol, 
                           int indexRow, int indexCol) {
   int midRow, midCol;
  
   if(level == 4)
      printf("Level 4\n");
   /*
    * If the box defined by (minRow, maxRow)X(minCol, maxCol) is outside
    * of the DEM then we return with a very negative number so that
    * this box does not influce the height of the parents box.
    *
    * Picture:
    * 
    * ______2^n___________________
    * |* * * * * * * * ..........|
    * |* * * * * * * * ..........|
    * |* *       * * * BBBBBB....|2^n
    * |* *  DEM  * * * BBBBBB....|
    * |* *       * * * BBBBBB....|
    * |* * * * * * * * ..........|
    * |* * * * * * * * ..........|
    * |* * * * * * * *...........|
    * |* * * * * * * *...........|
    * |* * * * * * * *...........|
    * |..........................|
    * |..........................|
    * |..........................|
    * |..........................|
    * |--------------------------|
    */
   if(indexRow >= getRowDim(tree->data, level) ||
      indexCol >= getColDim(tree->data, level)){
      return MIN_SHORT;
   } else if(level == getMaxLevel(tree->data)) {
      /*
       * Here we are at the DEM level 
       * (a.k.a., leaves, the smalllest boxes)
       * Picture:
       *
       * ______2^n___________________
       * |* * * * * * * * ..........|
       * |* * * * * * * * ..........|
       * |* *       * * * ..........|2^n
       * |* *  DEM  * * * ..........|
       * |* *       * * * ..........|
       * |* * * * * * * * ..........|
       * |* * * * * B * * ..........|
       * |* * * * * * * *...........|
       * |* * * * * * * *...........|
       * |* * * * * * * *...........|
       * |..........................|
       * |..........................|
       * |..........................|
       * |..........................|
       * |--------------------------|
       */
      if(maxRow >= getDEMRowDim(tree->data) ||
         maxCol >= getDEMColDim(tree->data)) {
         return MIN_SHORT;
      } else {
         // Compute the max height of the four DEM posts
         return maxElevation(
                  maxElevation(getDEMValue(tree->data, minRow, minCol),
                     getDEMValue(tree->data, maxRow, minCol)),
                  maxElevation(getDEMValue(tree->data, minRow, maxCol),
                     getDEMValue(tree->data, maxRow, maxCol)));
      }
   } else {
      
      // Compute the next mids
      midRow = ((maxRow - minRow) >> 1) + minRow;
      midCol = ((maxCol - minCol) >> 1) + minCol;


      return setValue(tree->data, level, indexRow, indexCol,
         maxElevation(
            maxElevation(recBuildQuadtree(tree, level + 1, minRow, midRow, 
                  minCol, midCol, indexRow * 2, indexCol * 2),
               recBuildQuadtree(tree, level + 1, minRow, midRow,
                  midCol, maxCol,  (indexRow * 2), (indexCol * 2) + 1)),
            maxElevation(recBuildQuadtree(tree, level + 1, midRow, maxRow,
                  minCol, midCol, (indexRow * 2) + 1, (indexCol * 2)),
               recBuildQuadtree(tree, level + 1, midRow, maxRow,
                  midCol, maxCol, (indexRow * 2) + 1, (indexCol * 2) + 1))));
   } // End of else if
} // End of recBuildQuadTree




/******************< getValueFromDEM Implementation >***********************
 * Already Defined in Header
 *    Implementation Comments:
 *     - None at this time
 */
Elevation getValueFromDEM(DEMPtr dem, int row, int column){
   return dem->data[row * dem->cols + column];
} // end of getValueFromDEM



/******************< setValueOnDEM Implementation >*************************
 * Already Defined in Header
 *    Implementation Comments:
 *     - None at this time
 */
Elevation setValueOnDEM(DEMPtr dem, int row, int column, 
         Elevation value){
   return (dem->data[row * dem->cols + column] = value);
} // end of setValueOnDEM
