/**
 **  indexcopy.c: Copy selected columns from one file to another
 **      Uses IBIS-2 Subroutine Library.
 **/
#include <string.h>
#include <ctype.h>
#include <stdlib.h>             //64-bit def of NULL

#include "vicmain_c.h"
#include "applic.h"
#include "indexmerge.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "carto/cartoMemUtils.h"      //for mz_alloc2
#include "carto/cartoStrUtils.h"
//#include "carto/cartoVicarProtos.h"

#define MAXCOLS 500
/*=========================================================*/

void main44(void)
{
    int i,icol,unit1,unit2,unit3,ibis1,ibis2,ibis3,ncol1,ncol2;
    int clen1,clen2,filewid,status,irow;
    int index,indcol1,indcol2,def;
    int startrow;
    char coltype1[MAXCOLS][6],coltype2[MAXCOLS][6];
    char outmsg[132];
    char **filedat1,**filedat2,**filedat3;

    double **numdat1,**numdat2;

	zvmessage ("*** indexmerge - 09-17-2013 (64-bit)  RJB ***"," ");    
    
   /* open the ibis interface files */

   status = zvunit(&unit1,"inp",1,NULL);            //64-bit
   status = IBISFileOpen(unit1,&ibis1,IMODE_READ,0,0,0,0);
   if (status!=1) IBISSignalU(unit1,status,1);
   IBISFileGet(ibis1,"nr",&clen1,1,1,0);
   IBISFileGet(ibis1,"nc",&ncol1,1,1,0);
   IBISFileGet(ibis1,"formats",coltype1,1,MAXCOLS,6);
   
   status = zvunit(&unit2,"inp",2,NULL);            //64-bit
   status = IBISFileOpen(unit2,&ibis2,IMODE_READ,0,0,0,0);
   if (status!=1) IBISSignalU(unit2,status,1);
   IBISFileGet(ibis2,"nr",&clen2,1,1,0);
   IBISFileGet(ibis2,"nc",&ncol2,1,1,0);
   IBISFileGet(ibis2,"formats",coltype2,1,MAXCOLS,6);

    if (clen2 > clen1) {
        zvmessage("??E 1st input file rows must be ge than 2nd input"," ");
        zabend();
    }
    sprintf (outmsg,"Number records file1 = %d",clen1);
    zvmessage(outmsg," ");
    sprintf (outmsg,"Number records file2 = %d",clen2);
    zvmessage(outmsg," ");

/* PARAMETERS */
    zvp("index",&index, &def);            /* tell which column index is in */
    indcol1=index-1;               /* adjust for c language 0-offset */
    indcol2=index-1;
    
    if (ncol1 != ncol2) {
        zvmessage("??E Num of cols in incols1 must = num of cols in incols2"," ");
        zabend();
    }
    
/* get Column Attributes from file 1 */
    for (icol=1;icol<=ncol1;icol++) {
       status = IBISColumnGet(ibis1,"FORMAT",coltype1[icol-1],icol);
       if (status!=1) IBISSignal(ibis1,status,1);
    }

/*   ncol3 = ncol1+ncol2;
   if (clen1>clen2) clen3 = clen1; else clen3 = clen2;
*/
    for (icol=1;icol<=ncol2;icol++) {
         status = IBISColumnGet(ibis2,"FORMAT",coltype2[icol-1],icol);
         if (status!=1) IBISSignal(ibis2,status,1);
    }

   /* OPEN OUTPUT file - same size and format as INPUT file 1*/

/*
    status = zvunit(&unit3,"out",1,0);
    status = IBISFileUnit(unit3,&ibis3,"write",ncol1,clen1,coltype1[icol-1],"column");
    status = IBISFileUnitOpen(ibis3);
*/

    status = zvunit(&unit3,"out",1,NULL);           //64-bit
    status = IBISFileOpen(unit3,&ibis3,"write",ncol1,clen1,0,0);
    if (status!=1) IBISSignalU(unit1,status,1);

    for (icol=0;icol<ncol1;icol++) {
        mz_alloc2((unsigned char ***)&numdat1,MAXCOLS,clen1,8);
    }
    for (icol=0;icol<ncol2;icol++) {
        mz_alloc2((unsigned char ***)&numdat2,MAXCOLS,clen1,8);
    }
    /* READ the data in file 1*/
   
    for (icol=0;icol<ncol1;icol++) {
        if (coltype1[icol][0]!='A') {    /* numeric column */
            status = IBISColumnSet(ibis1,"U_FORMAT","DOUB",icol+1);
            if (status!=1) IBISSignal(ibis1,status,1);
            status = IBISColumnRead(ibis1,(char *)numdat1[icol],icol+1,1,clen1);
            if (status!=1) IBISSignal(ibis1,status,1);

        } else {
                                        /* ASCII column */
            filewid = ms_num(&coltype1[icol][1])+1;
            mz_alloc2((unsigned char ***)&filedat1,MAXCOLS,clen1,filewid);
            mz_alloc2((unsigned char ***)&filedat3,MAXCOLS,clen1,filewid);
            status = IBISColumnRead(ibis1,filedat1[icol],icol+1,1,clen1);
        }
    }

    /* READ the data in file 2*/
    for (icol=0;icol<ncol2;icol++) {
        if (coltype2[icol][0]!='A') {    /* numeric column */
            status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",icol+1);
            if (status!=1) IBISSignal(ibis2,status,1);
            status = IBISColumnRead(ibis2,(char *)numdat2[icol],icol+1,1,clen2);
            if (status!=1) IBISSignal(ibis2,status,1);
        } else {
                                        /* ASCII column */
            filewid = ms_num(&coltype2[icol][1])+1;
            mz_alloc2((unsigned char ***)&filedat2,MAXCOLS,clen2,filewid);
            status = IBISColumnRead(ibis2,filedat2[icol],icol+1,1,clen2);
            if (status!=1) IBISSignal(ibis2,status,1);
        }
    }

/*  NOW, see what data to overwrite      */
/* Haven't set this up for ascii columns yet */
    startrow = 0;

    for (irow=0;irow<clen1;irow++) {                   /* thru each row */
        if (numdat1[indcol1][irow] == numdat2[indcol2][startrow]) {
            for (i=0;i<ncol2;i++) {
                 numdat1[i][irow] = numdat2[i][startrow];  /* replace all cols */
            }
            startrow++;
            if (startrow > clen2) goto done;
        }
    }
done:
    /* WRITE out the data to the output file*/

    for (icol=0;icol<ncol1;icol++) {
        if (coltype1[icol][0]!='A') {    /* numeric column */
            /* copy it to output */
            status = IBISColumnSet(ibis3,"U_FORMAT","DOUB",icol+1);
            if (status!=1) IBISSignal(ibis3,status,1);

            status = IBISColumnWrite(ibis3,(char *)numdat1[icol],icol+1,1,clen1);
            if (status!=1) IBISSignal(ibis3,status,1);
        } else {
            status = IBISColumnWrite(ibis3,filedat3[icol],icol+1,1,clen1);
            if (status!=1) IBISSignal(ibis3,status,1);
        }
	}

   status = IBISFileClose(ibis1,0);
   if (status!=1) IBISSignal(ibis1,status,1);
   status = IBISFileClose(ibis2,0);
   if (status!=1) IBISSignal(ibis2,status,1);
   status = IBISFileClose(ibis3,0);
   if (status!=1) IBISSignal(ibis3,status,1);

}

