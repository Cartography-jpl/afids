/*< EndeanTool.c >***********************************************************
 * The EndeanTool c file contains the implementation of various 
 * utility routines aid in endean manipulation
 *
 * Written by Joseph E. Gonzalez 
 * email: josephg@caltech.edu
 *
 */
#include <math.h>
#include "EndeanTool.h"

/***************************************************************************
 *                        < Method Implementations >                       *
 ***************************************************************************/

/***********************< getEndean Implementation >************************
 * Already Defined in header file
 * Comments:
 *    None
 */
Endean getEndean() {
   // Define a two byte field and set the higher ordered byte to 1
   unsigned short twoByteData = 0x0100;
   // access the first byte in the array of bytes if it is 1 then this
   // system is BIG_ENDEAN otherwise it is LITTLE_ENDEAN;
   return (*((char*)(&twoByteData)))? BIG_ENDEAN : LITTLE_ENDEAN;
} // End of getEndean()



/********************< toLittleEndean Implementation >**********************
 * Already Defined in header file
 * Comments:
 *    None
 */
void *toLittleEndean(void *ptr, size_t size, size_t nobj){
   char swap, *byteStream;
   unsigned int i,begin, end;

   if((size != 1) && getEndean()){
      // Assert this is big Endean so the byte order must be reversed
      byteStream = (char*)ptr;
      for(i = 0; i < (nobj * size); i += size) {
         for(begin = i, end = i + size - 1; begin < end; begin++, end--){
            swap = byteStream[begin];
            byteStream[begin] = byteStream[end];
            byteStream[end] = swap;
         } // End of copy for loop
      } // End of outer for loop
   } // End of if statement

   return ptr;
} // End of toLittleEndean



/**********************< toBigEndean Implementation >***********************
 * Already Defined in header file
 * Comments:
 *    None
 */
void *toBigEndean(void *ptr, size_t size, size_t nobj){
   char swap, *byteStream;
   unsigned int i,begin, end;
   
   if((size != 1) && (getEndean() == 0)){
      // Assert this is big Endean so the byte order must be reversed
      byteStream = (char*)ptr;
      for(i = 0; i < (nobj * size); i += size) {
         for(begin = i, end = i + size - 1; begin < end; begin++, end--){
            swap = byteStream[begin];
            byteStream[begin] = byteStream[end];
            byteStream[end] = swap;
         } // End of copy for loop
      } // End of outer for loop
   } // End of if statement
   return ptr;
} // End of toBigEndean



/********************< toNativeEndean Implementation >**********************
 * Already Defined in header file
 * Comments:
 *    None
 */
void *toNativeEndean(void *ptr, Endean endean, size_t size, size_t nobj){
   char swap, *byteStream;
   unsigned int i,begin, end;

   if((size != 1) && (getEndean() != endean)){
      byteStream = (char*)ptr;
      for(i = 0; i < (nobj * size); i += size) {
         for(begin = i, end = i + size - 1; begin < end; begin++, end--){
            swap = byteStream[begin];
            byteStream[begin] = byteStream[end];
            byteStream[end] = swap;
         } // End of copy for loop
      } // End of outer for loop
   } // End of if statement
   
   return ptr;
} // End of toNativeEndean


