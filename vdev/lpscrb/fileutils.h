#ifndef FILEUTILS_H
#define FILEUTILS_H

#include <stdlib.h>
#include <stdio.h>

FILE* openFile(char *, int);
int openIBISOut(int outFileParm, int nCol, int nRow);
void testPrint2(double** buf1, double** buf2, int row, int col);
int openIBISInp(int parm);
void getDimensions(int ibis, int *nc, int *nr);

#endif
