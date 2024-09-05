#include "applic.h"
#include <stdio.h>

#include "defines.h"
#include "fileutils.h"
#include "ibisfile.h"
#include "ibiserrs.h"

FILE* openFile(char* inpOrOut, int parm)
{
   int nb, ct, def;
   char infilename[99];

   zvp("NB", &nb, &ct);
   zvparm(inpOrOut, infilename, &ct, &def, parm, 0);

   return fopen(infilename, "r");
}

int openIBISOut(outFileParm, nRow, nCol)
   int outFileParm, nCol, nRow;
{
   int unit, ibis, status, i;
   zvselpi(0);
   zvunit(&unit, "out", outFileParm, 0);
   status = IBISFileUnit(unit, &ibis, "write", nCol, nRow, 0, 0);
   if(status != 1) IBISSignalU(unit, status, 1);
   printf("IBIS opening row: %d col: %d\n", nRow, nCol);
   status = IBISFileSet(ibis, "fmt_default", "doub", 1);
   if(status != 1) IBISSignal(ibis, status, 1);
   status = IBISFileUnitOpen(ibis);
   if(status != 1) IBISSignal(ibis, status, 1);

   return ibis;
}

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

int openIBISInp(parm)
   int parm;
{
   int status, unit, ibis;

   status = zvunit(&unit, "inp", parm, 0);
   if(status != 1) zmabend(status);
   printf("in subroutine unit before file open: %d parm: %d\n", unit, parm);
   status = IBISFileOpen(unit, &ibis, IMODE_READ, 0,0,0,0);
   if(status != 1) IBISSignalU(unit, status, 1);
   printf("in subroutine unit: %d ibis: %d\n", unit, ibis);

   return ibis;
}

void getDimensions(ibis, nc, nr)
   int *nc, *nr;
{
   int status;

   status = IBISFileGet(ibis, "nc", nc, 1, 0);
   if(status != 1) IBISSignal(ibis, status, 1);
   status = IBISFileGet(ibis, "nr", nr, 1, 0);
   if(status != 1) IBISSignal(ibis, status, 1);
}
