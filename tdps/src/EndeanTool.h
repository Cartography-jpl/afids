/*< EndeanTool.h >***********************************************************
 * The EndeanTool header file contains the defintions several routines
 * for manipulating the endean of binary data.
 *
 * Written by Joseph E. Gonzalez 
 * email: josephg@caltech.edu
 *
 */
#include <stdio.h>

#ifndef ENDEAN_TOOL
#define ENDEAN_TOOL


// Defin the Endean type 
typedef enum EndeanTag {LITTLE_ENDEAN, BIG_ENDEAN} Endean;

/***************************< getEndean Definition >************************
 * Arguments:
 *    - void
 * Return: 
 *    Endean: The native endean of the architecture on which this
 *       program is running.
 */
Endean getEndean();


/************************< toLittleEndean Definition >**********************
 * Arguments:
 *    - void *ptr : pointer to the begining of an array of objects   
 *    - size_t size : the size of each object in the array
 *    - size_t nobj : the number of objects in the array
 * Return: 
 *    - void* : the pointer that was passed in as ptr
 * Effect:
 *    - The endean of the system on which this program is running is
 *       determined.  If this system is a big endean system then
 *       the byte order of the array of objects is reversed
 *       otherwise it remains unchanged.
 */
void *toLittleEndean(void *ptr, size_t size, size_t nobj);


/************************< toBigEndean Definition >*************************
 * Arguments:
 *    - void *ptr : pointer to the begining of an array of objects   
 *    - size_t size : the size of each object in the array
 *    - size_t nobj : the number of objects in the array
 * Return: 
 *    - void* : the pointer that was passed in as ptr
 * Effect:
 *    - The endean of the system on which this program is running is
 *       determined.  If this system is a little endean system then
 *       the byte order of the array of objects is reversed
 *       otherwise it remains unchanged.
 */
void *toBigEndean(void *ptr, size_t size, size_t nobj);


/************************< toNativeEndean Definition >**********************
 * Arguments:
 *    - void *ptr : pointer to the begining of an array of objects   
 *    - Endean final : the endean of the endean native to the data
 *    - size_t size : the size of each object in the array
 *    - size_t nobj : the number of objects in the array
 * Return: 
 *    - void* : the pointer that was passed in as ptr
 * Effect:
 *    - The endean of the system on which this program is running is
 *       determined.  If this system is a different endean than the 
 *       native endean of the data then the byte order is flipped.
 */
void *toNativeEndean(void *ptr, Endean endean, size_t size, 
                    size_t nobj);

#endif


