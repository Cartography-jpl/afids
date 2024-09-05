#include "applic.h"
#include <stdio.h>

#include "VicHdrs.h"
#include "defines.h"
#include "fileutils.h"
#include "ibisfile.h"
#include "ibiserrs.h"

/**********************************************************/
FILE* openFile(char* inpOrOut, int parm)
{
   int nb, ct, def;
   char infilename[99];

   zvp("NB", &nb, &ct);
   zvparm(inpOrOut, infilename, &ct, &def, parm, 0);

   return fopen(infilename, "r");
}

/**********************************************************/
void getFileName(inpOrOut, parm, fileName)
   int inpOrOut, parm;
   char fileName[][99];
{
   int parmct, parmdf;

   if(inpOrOut == 1)
      zvparm("inp", fileName, &parmct, &parmdf, parm, 99);
   else if(inpOrOut == 2)
      zvparm("out", fileName, &parmct, &parmdf, parm, 99);
}

/**********************************************************/
int openIBISOut(outFileParm, nRow, nCol)
   int outFileParm, nCol, nRow;
{
   int unit, ibis, status, i;
   char fileNames[20][99];

   getFileName(IBIS_OUT, outFileParm, fileNames);

   zvselpi(0);
   zvunit(&unit, "out", outFileParm, 0);
   status = IBISFileUnit(unit, &ibis, "write", nCol, nRow, 0, 0);
   if(status != 1) IBISSignalU(unit, status, 1);
   printf("\t%s opening row: %d col: %d\n", fileNames[outFileParm-1], nRow, nCol);
   status = IBISFileSet(ibis, "fmt_default", "doub", 1);
   if(status != 1) IBISSignal(ibis, status, 1);
   status = IBISFileUnitOpen(ibis);
   if(status != 1) IBISSignal(ibis, status, 1);

   return ibis;
}

/**********************************************************/
/*
void testPrint2(buf1, buf2, row, col)
   double **buf1, **buf2;
   int row, col;
{
   int i, j;

   FILE *file = fopen("testoutput.txt", "w");

   for(i=0; i < row; i++)
   {
      for(j = 0; j < col; j++)
         fprintf(file, "%f %f ", buf1[i][j], buf2[i][j]);
      fprintf(file, "\n");
   }

   fclose(file);
}
*/

/**********************************************************/
int openIBISInp(parm)
   int parm;
{
   int status, unit, ibis;
   char fileNames[20][99];

   getFileName(IBIS_INP, parm, fileNames);
   status = zvunit(&unit, "inp", parm, 0);
   if(status != 1) zmabend("Error while getting input file unit.\n");
   status = IBISFileOpen(unit, &ibis, IMODE_READ, 0,0,0,0);
   if(status != 1) IBISSignalU(unit, status, 1);
   printf("\t%s opened unit: %d ibis: %d parm: %d\n", fileNames[parm-1], unit, ibis, parm);

   return ibis;
}

/**********************************************************/
void getDimensions(ibis, nc, nr)
   int *nc, *nr, ibis;
{
   int status;

   status = IBISFileGet(ibis, "nc", nc, 1, 1, 0);
   if(status != 1) IBISSignal(ibis, status, 1);
   status = IBISFileGet(ibis, "nr", nr, 1, 1, 0);
   if(status != 1) IBISSignal(ibis, status, 1);
}























