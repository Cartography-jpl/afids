/*< DataSource.c >*********************************************************
 * This contains the implementation of the data source header file

 *
 * Written by Joseph E. Gonzalez 
 * email: josephg@caltech.edu
 *
 */

#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include "DataSource.h"
#include "error_codes.h"



// This is the file offset at which the actual elevation data starts
const int DATA_OFFSET = 65536; //64kb
 
 /*******************< loadDataSource Implementation >**********************
 * Already Defined in header file
 * Comments:
 *    None
 */
DataSourcePtr loadDataSource(FILE *file, Endean fileEndean) {
   int i;
   DataSourcePtr source;
   
   printf("\nLoading Quadtree Data Source Binary Information\n");   
   // Allocate the memory for the source structure
   source = (DataSourcePtr)malloc(sizeof(DataSource));
   // Get the maxLevel information
   fread(&(source->maxLevel), sizeof(source->maxLevel), 1, file);
   // Correct the Endean on the Max Level
   toNativeEndean(&(source->maxLevel), fileEndean, 
      sizeof(source->maxLevel), 1);
   // Allocate memory for level dimensions array
   source->levelDim = (DimensionPtr)malloc(sizeof(Dimension) 
                                           * (source->maxLevel + 1));
   // Allocate memory for earthCurvatureOffset array
   source->earthCurvatureOffset = (int*) malloc(sizeof(int) 
                                               * (source->maxLevel + 1));
   // Allocate memory for the array of pointers to the arrays of data
   source->data = (Value**)malloc(sizeof(Value*) * (source->maxLevel + 1));
   // Read in the dimensions
   fread(source->levelDim, sizeof(Dimension), source->maxLevel + 1, file);
   // Loop through the dimensions and correct the Endean
   for(i = 0; i <= source->maxLevel; i++) {
      toNativeEndean(&(source->levelDim[i].rows), fileEndean, sizeof(int), 1);
      toNativeEndean(&(source->levelDim[i].cols), fileEndean, sizeof(int), 1);
   } // end of for loop
   // Read in the earthCurvatureOffset information
   fread(source->earthCurvatureOffset, sizeof(int), source->maxLevel + 1,
      file);
   // Correct the endean on the earthCurvatureOffset array
   toNativeEndean(source->earthCurvatureOffset, fileEndean, sizeof(int),
      source->maxLevel + 1);
   // Seek to the beginning of the actual elevation data
   fseek(file, DATA_OFFSET, SEEK_SET);
   for(i = 0; i <= source->maxLevel; i++) {
      // Allocate the large amount of memory for each level of data
      source->data[i] = (Value*)malloc(sizeof(Value) 
                                       * source->levelDim[i].rows 
                                       * source->levelDim[i].cols);

      // Fill the level of data with the data stored in the file
      fread(source->data[i], sizeof(Value),
         source->levelDim[i].rows * source->levelDim[i].cols, file);

      // Correct the endean of the data
      toNativeEndean(source->data[i], fileEndean, sizeof(Value),
         source->levelDim[i].rows * source->levelDim[i].cols);
   } // End of for loop
   return source;
} // End of openDataSource



/*****************< writeDataSourceHeader Implementation >******************
 * Already Defined in header file
 * Comments:
 *    None
 */
void writeDataSourceHeader(FILE *file, const DataSourcePtr source) {
   int i;
   printf("\nWriting Quadtree Data Source Header Information\n");
   // Write header information as text
   fprintf(file, "\nThe following information pertains to the structure ");
   fprintf(file, "of the data source.\n");
   fprintf(file, "Max Level:\t\t%d\n", source->maxLevel);
   fprintf(file, "Level Number\tDimension (rows,cols)\t");
   fprintf(file, "Earth Curvature Offset\n");
   for(i = 0; i <= source->maxLevel; i++)
      fprintf(file, "%d\t\t(%10d,%10d)\t%d\n", i, 
              source->levelDim[i].rows, source->levelDim[i].cols,
              source->earthCurvatureOffset[i]);
} // End of writeDataSourceHeader




/*********************< saveDataSource Implementation >*********************
 * Already Defined in header file
 * Comments:
 *    None
 */
void saveDataSource(FILE *file, DataSourcePtr source, Endean fileEndean) {
   int i, maxLevel;
   DimensionPtr levelDim;
   printf("\nWriting Quadtree DataSource Binary information");
   // Save a copy of the maxLevel field because the endean correction
   // routiens may destroy the value of source->maxLevel
   maxLevel = source->maxLevel;
   // Allocate memory to temporarily store the level dimensions
   levelDim = (DimensionPtr)malloc(sizeof(Dimension) * (maxLevel + 1));
   // Write the maxLevel field to disk.  This process may modify
   // the value if the Endean requires correction
   fwrite(toNativeEndean(&(source->maxLevel), fileEndean, 
      sizeof(source->maxLevel), 1), sizeof(source->maxLevel), 1, file);
   // Loop through the dimensions and correct the endean
   // This process could alter the value if the Endean requires correction
   for(i = 0; i <= maxLevel; i++) {
      levelDim[i].rows = source->levelDim[i].rows;
      levelDim[i].cols = source->levelDim[i].cols;
      toNativeEndean(&(source->levelDim[i].rows), fileEndean, 
         sizeof(int), 1);
      toNativeEndean(&(source->levelDim[i].cols), fileEndean, 
         sizeof(int), 1);
   } // end of for loop
   // Write the level dimensions to the file
   fwrite(source->levelDim, sizeof(Dimension), maxLevel + 1, file);
   // Write the earth curvature offset to the file
   fwrite(toNativeEndean(source->earthCurvatureOffset, fileEndean, 
      sizeof(int), maxLevel + 1), sizeof(int), maxLevel + 1, file);
   // Seek into the file to begin writing the actual elevation data
   fseek(file, DATA_OFFSET, SEEK_SET);
   // Write the elevation data
   for(i = 0; i <= maxLevel; i++)
      fwrite(toNativeEndean(source->data[i], fileEndean, sizeof(Value), 
             levelDim[i].rows * levelDim[i].cols), sizeof(Value),
             levelDim[i].rows * levelDim[i].cols, file);
   // Destroy the temporary levelDim array 
   free(levelDim);
} // End of saveDataSource




/**********************< getMaxLevel Implementation >***********************
 * Already Defined in header file
 * Comments:
 *    None
 */
int getMaxLevel(const DataSourcePtr source) { return source->maxLevel; }



/***********************< getRowDim Implementation >************************
 * Already Defined in header file
 * Comments:
 *    None
 */
int getRowDim(const DataSourcePtr source, int level) { 
   return source->levelDim[level].rows;
} // End of getRowDim



/***********************< getColDim Implementation >************************
 * Already Defined in header file
 * Comments:
 *    None
 */
int getColDim(const DataSourcePtr source, int level) { 
   return source->levelDim[level].cols;
} // End of getColDim



/************************< getDEMRowDim Implementation >********************
 * Already Defined in header file
 * Comments:
 *    None
 */
int getDEMRowDim(const DataSourcePtr source) { 
   return source->levelDim[source->maxLevel].rows;
} // End of getDEMRowDim



/************************< getDEMColDim Implementation >********************
 * Already Defined in header file
 * Comments:
 *    None
 */
int getDEMColDim(const DataSourcePtr source) { 
   return source->levelDim[source->maxLevel].cols;
} // End of getDEMColDim



/********************< getEarthCurveOffset Implementation >*****************
 * Already Defined in header file
 * Comments:
 *    None
 */
int getEarthCurveOffset(const DataSourcePtr source, int level) {
   return source->earthCurvatureOffset[level];
} // End of getEarthCurveOffset



/*************************< getValue Implementation >***********************
 * Already Defined in header file
 * Comments:
 *    None
 */
Value getValue(const DataSourcePtr source, int level, int row, int col){
   
//   assert((row < source->levelDim[level].rows)
//      && (col < source->levelDim[level].cols)
//      && (level <= source->maxLevel));
   if ( !((row < source->levelDim[level].rows) &&
         (col < source->levelDim[level].cols) &&
                          (level <= source->maxLevel)) )
     return ( ERROR_CODES );
   return source->data[level][row * source->levelDim[level].cols + col];
} // end of getValue


/************************< getDEMValue Implementation >*********************
 * Already Defined in header file
 * Comments:
 *    None
 */
Value getDEMValue(const DataSourcePtr source, int row, int col){
//   assert((row < source->levelDim[source->maxLevel].rows)
//      && (col < source->levelDim[source->maxLevel].cols));
   if ( !((row < source->levelDim[source->maxLevel].rows) &&
          (col < source->levelDim[source->maxLevel].cols)) )
     return ( ERROR_CODES );
  return source->data[source->maxLevel]
      [row * source->levelDim[source->maxLevel].cols + col];
} // end of getValue


/**********************< setValue Implementation >**************************
 * Already Defined in header file
 * Comments:
 *    None
 */
Value setValue(DataSourcePtr source, int level, 
                          int row, int col, Value value) {
   assert((row < source->levelDim[level].rows) 
      && (col < source->levelDim[level].cols)
      && (level <= source->maxLevel));
   return (source->data[level][row * source->levelDim[level].cols + col] 
      = value);
} // end of setValue


/****************< displayDataSourceHeader Implementation >*****************
 * Already Defined in header file
 * Comments:
 *    None
 */
void displayDataSourceHeader(const DataSourcePtr source){
   int i;
   printf("\n\nData Source Information\n\n");
   printf("Level Number\tDimension (rows,cols)\tEarth Curvature Offset\n");
   for(i = 0; i <= source->maxLevel; i++)
      printf("%d\t\t(%10d,%10d)\t%d\n", i, source->levelDim[i].rows, 
         source->levelDim[i].cols, source->earthCurvatureOffset[i]);   
} // end of displayDataSourceHeader



