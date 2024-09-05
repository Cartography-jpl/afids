#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <assert.h>

#include "carto/ImageUtils.h"
#include "carto/cartoGtUtils.h"
#include "carto/cartoSortUtils.h"
#include "carto/ibishelper.h"

typedef struct{
   int sl, ss, nl, ns, id;
   double **buf;
} CHIP;

/***************************************************************/
CHIP* createChip(int x, int y, int nl, int ns, int id, int chipSize)
{
   int i;
   CHIP *chip;

   chip = (CHIP*)malloc(sizeof(CHIP));
   chip->ss = (x-chipSize/2>=0)?x-chipSize/2:0;
   chip->ns = (chip->ss+chipSize<ns)?chipSize:ns-chip->ss;
   chip->sl = (y-chipSize/2>=0)?y-chipSize/2:0;
   chip->nl = (chip->sl+chipSize<nl)?chipSize:nl-chip->sl;
   chip->id = id;
   chip->buf = (double**)calloc(chipSize, sizeof(double));
   for(i = 0; i < chipSize; i++)
      chip->buf[i] = (double*)calloc(chipSize, sizeof(double));

   //   printf("x: %d y: %d\n", x, y);
   //   printf("%d %d %d %d %d\n", chip->id, chip->ss, chip->ns, chip->sl, chip->nl);

   return chip;
}

/***************************************************************/
void destroyChip(CHIP **chip)
{
   int i;

   for(i = 0; i < (*chip)->nl; i++)
      free((*chip)->buf[i]);
   free((*chip)->buf);

   free(*chip);
}

/***************************************************************/
/* return 0 if buffer given is outside the chip domain         */
/* return 1 if any line except the last chip line is filled    */
/* return 2 if the last chip line is filled                    */
/***************************************************************/
int chipFill(CHIP *chip, double *buf, int line, int chipSize)
{
   int chipLine, i;

   if(line < chip->sl || line >= chip->sl+chip->nl) return 0;

   chipLine = line - chip->sl;
   //   printf("id: %d line: %d ns: %ld\n", chip->id, chipLine, chip->ns*sizeof(double));
   for(i = 0; i < chip->ns; i++) chip->buf[chipLine][i] = buf[chip->ss+i];
   //   memcpy(&(chip->buf[chipLine]), &(buf[chip->ss]), chip->ns*sizeof(double));
   //   printf("successfully copied\n");

   /* assume chip is filled if last line of the chip is filled */
   if(chipLine == chipSize-1) return 2;
   return 1;
}

/***************************************************************/
void registerLinesToRead(CHIP **chips, int *lineRegister, int nChips)
{
   int i, j;

   for(i = 0; i < nChips; i++)
   {
      int sl = chips[i]->sl;
      int ll = chips[i]->sl + chips[i]->nl;

      //      printf("sl: %d ll: %d\n", sl, ll);
      for(j = sl; j < ll; j++) lineRegister[j] = 1;
   }
}

/***************************************************************/
void outBorderRow(VICAR_IMAGE *out, int row, int border, int chipSize)
{
   int j, k, startLine, endLine;

   startLine = row*chipSize + row*border;
   endLine = startLine + border;
   for(j = startLine; j < endLine; j++)
   {
      for(k = 0; k < out->ns; k++) out->buffer[k] = 0.;
      writeVicarImageLine(out, j);
   }
}

/***************************************************************/
void outChips(VICAR_IMAGE *out, CHIP **subChips, int row, int nSubChips,
              int chipsPerRow, int border, int chipSize)
{
   int i, startLine;

   startLine = row*chipSize + (row + 1)*border;
   // iterate over lines
   for(i = 0; i < chipSize; i++)
   {
      int j, k, outIndex, bufIndex;

      for(j = 0; j < nSubChips; j++)
      {
         outIndex = j*chipSize + j*border;
         for(k = outIndex; k < outIndex + border; k++)
            out->buffer[k] = 0.;
         outIndex += border;
         bufIndex = 0;
         for(k = outIndex; k < outIndex + chipSize; k++)
            out->buffer[k] = subChips[j]->buf[i][bufIndex++];
      }
      outIndex += chipSize;

      // create black spaces if nSubChips is less than chipsPerRow and write last border
      while(outIndex < out->ns)
         out->buffer[outIndex++] = 0.;

      writeVicarImageLine(out, i+startLine);
   }
}

/***************************************************************/
int getChipsInRow(CHIP **chips, CHIP **subChips, int row, int chipsInRow, int border, int nChips)
{
   int i, startChip;

   startChip = row*chipsInRow;
   for(i = 0; i < chipsInRow && startChip+i < nChips; i++)
      subChips[i] = chips[startChip+i];

   return i;
}

/***************************************************************/
void createOutputImage(CHIP **chips, VICAR_IMAGE *out, int nChips, int chipsPerRow,
                       int chipSize, int border)
{
   int row, endRow, nSubChips;
   CHIP **subChips;

   subChips = (CHIP**)malloc(sizeof(CHIP*)*chipsPerRow);

   row = 0;
   endRow = nChips/chipsPerRow;
   if(nChips%chipsPerRow > 0) ++endRow;
   while(row < endRow)
   {
     //      int i;
      outBorderRow(out, row, border, chipSize);
      nSubChips = getChipsInRow(chips, subChips, row, chipsPerRow, border, nChips);

      //      for(i = 0; i < nSubChips; i++) printf("%d: %d %d %d %d\n", subChips[i]->id, subChips[i]->sl, subChips[i]->ss, subChips[i]->nl, subChips[i]->ns);

      outChips(out, subChips, row, nSubChips, chipsPerRow, border, chipSize);
      ++row;
   }
   outBorderRow(out, row, border, chipSize);

   deleteAndCloseImage(&out);
   free(subChips);
}

/***************************************************************/
void createOutputImage2(CHIP ***chips, char *fmt, int nChips, int inpCnt,
                        int chipSize, int nl, int ns, int border, int outInst)
{
   int i, reorderCnt;
   CHIP **reorderedChips;
   VICAR_IMAGE *out;

   out = getVI_out(fmt, outInst, nl, ns);
   reorderedChips = (CHIP**)malloc(sizeof(CHIP*)*nChips*inpCnt);
   reorderCnt = 0;
   for(i = 0; i < nChips; i++)
   {
      int j;

      for(j = 0; j < inpCnt; j++)
         reorderedChips[reorderCnt++] = chips[j][i];
   }
   // sanity check
   assert(reorderCnt == nChips*inpCnt);

   createOutputImage(reorderedChips, out, nChips*inpCnt, inpCnt, chipSize, border);
   free(reorderedChips);
}

/***************************************************************/
void createOutputImage3(CHIP ***chips, char *fmt, int nChips, int chipsPerRow,
                        int chipSize, int inpStack, int nl, int ns, int border)
{
   int i;
   VICAR_IMAGE *out;

   for(i = 0; i < inpStack; i++)
   {
      out = getVI_out(fmt, i+1, nl, ns);
      createOutputImage(chips[i], out, nChips, chipsPerRow, chipSize, border);
   }
}

/***************************************************************/
void createOutputImage4(CHIP ***chips, int *inpStack, char *fmt, int stackCnt, int maxCnt,
                        int nChips, int chipsPerRow, int chipSize, int nl, int ns, int border)
{
   int i;
   CHIP ***stackChips;

   stackChips = (CHIP***)malloc(sizeof(CHIP**)*stackCnt);
   for(i = 0; i < maxCnt; i++)
   {
      int j;
      int chipIndex, stackIndex;

      chipIndex = stackIndex = 0;
      for(j = 0; j < stackCnt; j++)
      {
         int prevIndex = chipIndex;

         if(inpStack[j] > i)
            chipIndex += i;
         else chipIndex += inpStack[j] - 1;
         stackChips[stackIndex++] = chips[chipIndex];
         chipIndex = prevIndex + inpStack[j];
      }

      createOutputImage2(stackChips, fmt, nChips, stackCnt, chipSize, nl, ns, border, i+1);
   }

   free(stackChips);
}

/***************************************************************/
void initializeText()
{
   int status;

   status = ztxtfont(1);
   assert(status == 1);
   status = ztxtrotate(0);
   assert(status == 1);
   status = ztxtsize(20,0.5);
   assert(status == 1);
   status = ztxtcolor(1);
   assert(status == 1);
}

/***************************************************************/
void chipTextOverlay(CHIP *chip, unsigned char* overlay)
{
   int i, j, nl, ns;

   nl = chip->nl;
   ns = chip->ns;

   for(i = 0; i < nl; i++)
   {
      for(j = 0; j < ns; j++)
      {
         int dn = overlay[i*ns+j];

         if(dn == 0) continue;

         chip->buf[i][j] = (double)dn;
      }
   }
}

/***************************************************************/
void chipText(CHIP ***chips, IBISStruct *ibis, int inpCnt, int textCol)
{
   int i, j, colLen, flag, status;
   char *text;
   unsigned char *txtBuf;

   textCol--;
   colLen = ibis->colLens[textCol];
   text = calloc(colLen, sizeof(char));

   txtBuf = (unsigned char*)calloc(chips[0][0]->nl*chips[0][0]->ns, sizeof(char));

   for(i = 0; i < inpCnt; i++)
   {
      for(j = 0; j < ibis->nr; j++)
      {
         // reset string and buffer
         text = memset(text, '\0', colLen);
         txtBuf = memset(txtBuf, '\0', chips[0][0]->nl*chips[0][0]->ns);

         IBISHELPER_getString(ibis, text, textCol, j);

         status = ztxttext(txtBuf, chips[i][j]->nl, chips[i][j]->ns, chips[i][j]->ns/2,
                           chips[i][j]->nl/10, 2, strlen(text), text, &flag);
         chipTextOverlay(chips[i][j], txtBuf);
         assert(status == 1);
      }
   }
}

/***************************************************************/
void getMapping(double *t)
{
   int status, labnl, labns, i;
   char *labelstr;
   double tinv[6], corner[4];

   /* calculate the mapping */
   status = gtgetlab("inp",2,&labelstr,&labnl,&labns);
   status = geofix(labelstr,t,tinv,labnl,labns,corner);
   if (status!=1) zmabend("Failed to get mapping from GeoTIFF label");
   for (i=0;i<6;i++) t[i] = tinv[i];
}

/***************************************************************/
void getXY(double *t, double east, double north, int *x, int *y)
{
   /* calculate the output data */
   *y = (int)(east*t[0]+north*t[1]+t[2]);
   *x = (int)(east*t[3]+north*t[4]+t[5]);
}

/***************************************************************/
void main44(void)
{
   int i, j, k, status, dumcnt, dumdef, cols[2], inpCnt, outCnt, outNL, outNS;
   int chipsPerRow, chipSize, inpStack[9], stackCnt, maxStack, stackSum, textCol;
   VICAR_IMAGE **inps;
   double t[6];
   int *x, *y, *sortIndices, *linesToRead, firstUnfilledChip, border;
   IBISStruct *ibis;
   CHIP ***chips;

   /* input check */
   status = zvparm("inpstack", inpStack, &dumcnt, &dumdef, 9, 0);
   assert(status == 1);
   if(inpStack[0] == 0) stackCnt = 0;
   else
   {
      status = zvpcnt("inpstack", &stackCnt);
      assert(status == 1);
   }
   maxStack = 1;
   stackSum = 0;
   for(i = 0; i < stackCnt; i++)
   {
      if(inpStack[i] > maxStack) maxStack = inpStack[i];
      stackSum += inpStack[i];
   }
   status = zvpcnt("out", &outCnt);
   if(maxStack != outCnt)
      zmabend("The number of outputs must match the largest inpStack.");
   status = zvpcnt("inp", &inpCnt);
   assert(status == 1);
   --inpCnt;
   if(stackCnt > 0 && stackSum != inpCnt)
      zmabend("The number of input images must match the sum of inpStack.");

   /* read data from ibis file and convert to x y coordinates */
   ibis = IBISHELPER_openIBIS("inp", 1, "read");
   x = (int*)malloc(sizeof(int)*ibis->nr);
   y = (int*)malloc(sizeof(int)*ibis->nr);
   status = zvparm("cols", cols, &dumcnt, &dumdef, 2, 0);
   assert(status == 1);

   getMapping(t);
   for(i = 0; i < ibis->nr; i++)
   {
      double east, north;

      east = IBISHELPER_getDouble(ibis, cols[0]-1, i);
      north = IBISHELPER_getDouble(ibis, cols[1]-1, i);

      getXY(t, east, north, &(x[i]), &(y[i]));
      //      printf("%lf %lf %d %d\n", east, north, x[i], y[i]);
   }

   /* open input image and allocate memory for chips */
   status = zvparm("chipsize", &chipSize, &dumcnt, &dumdef, 1, 0);
   assert(status == 1);
   chips = (CHIP***)malloc(sizeof(CHIP**)*inpCnt);

   inps = (VICAR_IMAGE**)malloc(sizeof(VICAR_IMAGE*)*inpCnt);
   for(j = 0; j < inpCnt; j++)
   {
      inps[j] = getVI_inp(j+2);
      chips[j] = (CHIP**)malloc(sizeof(CHIP*)*ibis->nr);
      for(k = 0; k < ibis->nr; k++)
         chips[j][k] = createChip(x[k], y[k], inps[0]->nl, inps[0]->ns, k, chipSize);
   }

   //   printf("input image nl: %d ns: %d\n", inps[0]->nl, inps[0]->ns);

   /* optimize by only reading necessary lines */
   linesToRead = (int*)calloc(inps[0]->nl, sizeof(int));
   registerLinesToRead(chips[0], linesToRead, ibis->nr);

   /* optimize by sorting lines to read */
   sortIndices = (int*)malloc(sizeof(int)*ibis->nr);
   getSelectionSortIndices(y, sortIndices, ibis->nr, CART_INT);
   //   for(i = 0; i < ibis->nr; i++) printf("%d %d\n", sortIndices[i], y[sortIndices[i]]);

   /* fill the chips */
   firstUnfilledChip = 0;
   for(i = 0; i < inps[0]->nl; i++)
   {
      if(!linesToRead[i]) continue;

      for(j = 0; j < inpCnt; j++)
      {
         readVicarImageLine(inps[j], i);

         for(k = firstUnfilledChip; k < ibis->nr; k++)
         {
            int fillStatus;

            fillStatus = chipFill(chips[j][sortIndices[k]], inps[j]->buffer, i, chipSize);
            if(fillStatus == 2 && j == inpCnt - 1) firstUnfilledChip = k+1;
            if(fillStatus == 0) break;
         }
      }
   }

   /* write text if specified */
   status = zvparm("textcol", &textCol, &dumcnt, &dumdef, 1, 0);
   assert(status == 1);
   if(textCol > 0)
   {
      printf("*Drawing Text\n");
      initializeText();
      chipText(chips, ibis, inpCnt, textCol);
   }

   /* create output file */
   zvselpi(2);
   status = zvparm("BORDER", &border, &dumcnt, &dumdef, 1, 0);
   assert(status == 1);

   if((stackCnt == 0 && inpCnt == 1) || stackCnt == 1)
   {
      status = zvparm("CHIPROW", &chipsPerRow, &dumcnt, &dumdef, 1, 0);
      assert(status == 1);
      outNL = chipSize*(ibis->nr/chipsPerRow) + border*(ibis->nr/chipsPerRow + 1);
      if(ibis->nr%chipsPerRow > 0) outNL += chipSize + border;
   }
   else if(stackCnt > 1)
   {
      chipsPerRow = stackCnt;
      outNL = chipSize*ibis->nr + border*(ibis->nr + 1);
   }
   else
   {
      chipsPerRow = inpCnt;
      outNL = chipSize*ibis->nr + border*(ibis->nr + 1);
   }

   outNS = chipSize*chipsPerRow + border*(chipsPerRow+1);
   if(maxStack > 1)
   {
      if(stackCnt == 1)
      {
         int dumcnt, dumdef;

         status = zvparm("CHIPROW", &chipsPerRow, &dumcnt, &dumdef, 1, 0);
         assert(status == 1);
         createOutputImage3(chips, inps[0]->format, ibis->nr, chipsPerRow,
                            chipSize, inpStack[0], outNL, outNS, border);
      }
      else
         createOutputImage4(chips, inpStack, inps[0]->format, stackCnt, maxStack,
                            ibis->nr, chipsPerRow, chipSize, outNL, outNS, border);
   }
   else if(inpCnt == 1)
   {
      VICAR_IMAGE *out;

      out = getVI_out(inps[0]->format, 1, outNL, outNS);
      createOutputImage(chips[0], out, ibis->nr, chipsPerRow, chipSize, border);
   }
   else createOutputImage2(chips, inps[0]->format, ibis->nr, inpCnt, chipSize, outNL, outNS, border, 1);

   // free allocated memories
   free(x);
   free(y);

   for(i = 0; i < inpCnt; i++)
   {
      for(j = 0; j < ibis->nr; j++)
         destroyChip(&(chips[i][j]));

      free(chips[i]);
      deleteAndCloseImage(&(inps[i]));
   }
   free(chips);

   IBISHELPER_closeIBIS(&ibis);
   free(linesToRead);
   free(sortIndices);
   free(inps);
}
