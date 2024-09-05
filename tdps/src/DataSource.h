/*< DataSource.h >***********************************************************
 * The data source header file contains the basic definitions of the
 * structures used to store the 3 dimensional matrices which contain
 * the quadtree elevation data.  The header file also defines
 * the routines used to access data from a data source.
 * The data source is intended to behave like an abstract
 * data type. 
 *
 * Written by Joseph E. Gonzalez 
 * email: josephg@caltech.edu
 *
 */

#include <stdio.h>
#include "EndeanTool.h"

#ifndef DATA_SOURCE
#define DATA_SOURCE

typedef short int Value; // The number type stored in this data source

/***************************************************************************
 *                              < Structures >                             *
 ***************************************************************************/
/*
 * The Dimension structure defines the dimensions of a matrix
 */
typedef struct {
   int rows, cols;
} Dimension, *DimensionPtr;

/*
 * The data source structures is an abstraction of the data source 
 *    used in the Quadtree algorithm
 */
typedef struct{
   int maxLevel;        // Index of the maximum levels in the matrix
   DimensionPtr levelDim;  // An array of dimensions
   
   // The array of offset for each level in the tree that compensate 
   // for the curvature of the earth
   int *earthCurvatureOffset;

   // A pointer to the array of pointers to each level in the tree
   Value **data; 
} DataSource, *DataSourcePtr;



/***************************************************************************
 *                               < Routines >                              *
 ***************************************************************************/

/**********************< loadDataSource Definition >************************
 * Arguments:
 *    - FILE *file: The pointer to a file stream.
 * Return: 
 *    - DataSourcePtr: the pointer to an initialized data source
 * Details:
 *    This will be called to load an existing tree from a file.  This will
 *    allocate the memory needed for the data source.  
 */
DataSourcePtr loadDataSource(FILE *file, Endean fileEndian);



/******************< writeDataSourceHeader Definition >*********************
 * Arguments:
 *    - FILE *file: The pointer to a file stream.
 *    - DataSourcePtr source: the pointer to the data source
 * Return: 
 *    void
 * Details:
 *    This will print a description of the DataSource into the file in
 *    ascii text form.
 */
void writeDataSourceHeader(FILE *file, const DataSourcePtr source);



/**********************< saveDataSource Definition >************************
 * Arguments:
 *    - FILE *file: The pointer to a file stream.
 *    - DataSourcePtr source: the pointer to the data source
 *    - int fileEndean: the desired endean of the data in the file
 * Return: 
 *    - void
 * Details:
 *    This saves all of the information stored in the DataSource to the
 *    file.  The endean of that information is first converted to the
 *    endean specified by fileEndean.  The endean conversion process 
 *    modifies the the data source.  
 */
void saveDataSource(FILE *file, DataSourcePtr source, Endean fileEndean);



/**********************< getMaxLevel Definition >***************************
 * Arguments:
 *    - DataSourcePtr source: The pointer to a data source structure
 * Return: 
 *    - int: The index of the last level in the data source
 * Details:
 *    This should be FAST!
 */
int getMaxLevel(const DataSourcePtr source);



/***********************< getRowDim Definition >****************************
 * Arguments:
 *    - DataSourcePtr source: The pointer to a data source structure
 *    - int level: The level in the tree
 * Return: 
 *    - int: The dimension of a row on that level of the data source
 */
int getRowDim(const DataSourcePtr source, int level);


/***********************< getColDim Definition >****************************
 * Arguments:
 *    - DataSourcePtr source: The pointer to a data source structure
 *    - int level: The level in the tree
 * Return: 
 *    - int: The dimension of a column on that level of the data source
 */
int getColDim(const DataSourcePtr source, int level);


/************************< getDEMRowDim Definition >************************
 * Arguments:
 *    - DataSourcePtr source: The pointer to a data source structure
 * Return: 
 *    - int: The dimension of a row on the DEM (the last level of the 
 *       data source)
 */
int getDEMRowDim(const DataSourcePtr source);


/************************< getDEMColDim Definition >************************
 * Arguments:
 *    - DataSourcePtr source: The pointer to a data source structure
 * Return: 
 *    - int: The dimension of a column on the DEM (the last level 
 *             of the data source)
 */
int getDEMColDim(const DataSourcePtr source);



/******************< getEarthCurveOffset Definition >***********************
 * Arguments:
 *    - DataSourcePtr source: The pointer to a data source structure
 * Return: 
 *    - int: The dimension of a column on the DEM (the last level 
 *             of the data source)
 */
int getEarthCurveOffset(const DataSourcePtr source, int level);



/*************************< getValue Definition >***************************
 * Arguments:
 *    - DataSourcePtr source: The pointer to a data source structure
 *    - int level: The level in the tree
 *    - int row: the row in the matrix on that level
 *    - int col: the column in the matrix on that level
 * Return: 
 *    - Value: the value stored in the element addressed by the
 *       arguments.
 */
Value getValue(const DataSourcePtr source, int level, int row, int col);


/************************< getDEMValue Definition >*************************
 * Arguments:
 *    - DataSourcePtr source: The pointer to a data source structure
 *    - int row: the row in the matrix on that level
 *    - int col: the column in the matrix on that level
 * Return: 
 *    - Value: the value stored in the max level (DEM) level 
 *       addressed by the arguments.
 */
Value getDEMValue(const DataSourcePtr source, int row, int col);


/**********************< setValue Definition >******************************
 * Arguments:
 *    - DataSourcePtr source: The pointer to a data source structure
 *    - int level: The level in the tree
 *    - int row: the row in the matrix on that level
 *    - int col: the column in the matrix on that level
 * Return: 
 *    - Void
 * Effect:
 *    - Modify the data source such that a subsequent 
 *       invocation of the getValue routine passing the 
 *       level, row, and column should produce the result value.
 */
Value setValue(DataSourcePtr source, int level, 
               int row, int col, Value value);





/*******************< displayDataSourceHeader Defintion >*******************
 * Arguments:
 *    - DataSourcePtr data -> The pointer to a DataSource structure
 * Return: 
 *    - void
 * Effect:
 *    - Displays the DataSource description information
 */
void displayDataSourceHeader(const DataSourcePtr source);




#endif


