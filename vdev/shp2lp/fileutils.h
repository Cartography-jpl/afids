#ifndef FILEUTILS_H
#define FILEUTILS_H

#include <stdlib.h>
#include <stdio.h>

#define IBIS_INP 1
#define IBIS_OUT 2

/***********************************************
fileutils.h

Written by Peter Kim

Aug 02 2007: Documented for testing and finalizing. pk
***********************************************/

/******************************************
This function opens a file and returns
a file handle.  Pass in "inp" or "out"
for input or output and also the parameter
from the proc call.
******************************************/
FILE* openFile(char *inpOrOut, int parm);

/******************************************
This function puts the file name from the
proc call into fileName.  Pass in IBIS_INP
for input file and IBIS_OUT for output file
and parameter from the proc call.
******************************************/
void getFileName(int inpOrOut, int parm, char fileName[][99]);

/******************************************
This function returns and ibis number and
sets up the IBIS file for output as
specified by the nCol and nRow.  Pass in
the parameter from the proc call to determine
which file to open.
NOTE: It sets double as default column format
type.
******************************************/
int openIBISOut(int outFileParm, int nCol, int nRow);

/******************************************
This function returns an ibis number and
sets up IBIS file for reading.  Simply pass
in the parameter number from the proc call.
******************************************/
int openIBISInp(int parm);

/******************************************
This function gets the number of columns
and number of rows from the specified
IBIS file.
******************************************/
void getDimensions(int ibis, int *nc, int *nr);

#endif






