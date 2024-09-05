#include "applic.h"
#include "zvproto.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "cloud_masks.h"
#include "carto/ImageUtils.h"
#include "LandsatManager.h"
#include "ACCA_Utils.h"

/******************************************************************************/
void openMaskFile(int *unit, char parmName[IU_MAX_FNAME_LEN], int nl, int ns, int inst)
{
   int cnt, def, status;
   char fname[250];

   if(!strcmp(parmName, "out"))
   {
      status = zvunit(unit, "out", 1, NULL);
      assert(status == 1);
   }
   else
   {
      status = zvparm(parmName, fname, &cnt, &def, 1, IU_MAX_FNAME_LEN);
      assert(status == 1);
      if(cnt)
      {
         status = zvunit(unit, "MASK", inst, "U_NAME", fname, NULL);
         assert(status == 1);
      }
      else
      {
         *unit = -1;
         return;
      }
   }

   status = zvopen(*unit, "OP", "WRITE", "U_FORMAT", "BYTE", "O_FORMAT", "BYTE", "U_NL", nl, "U_NS", ns, NULL);
   assert(status == 1);
}

/******************************************************************************/
void closeMaskFile(int unit)
{
   int status;

   status = zvclose(unit, NULL);
   printf("unit: %d status: %d\n", unit, status);
   assert(status == 1);
}

/******************************************************************************/
void openFile(int *unit, char *name, char *fname, char *mode, char *oformat, int inst, int nl, int ns)
{
   int status;

   if(!strcmp(name, "inp") || !strcmp(name, "out"))
   {
      status = zvunit(unit, name, inst, NULL);
      assert(status == 1);
   }
   else
   {
      status = zvunit(unit, name, inst, "U_NAME", fname, NULL);
      assert(status == 1);
   }

   if(!strcmp(name, "inp") || !strcmp(mode, "read"))
      status = zvopen(*unit, "OP", mode, "U_FORMAT", "DOUB", NULL);
   else
      status = zvopen(*unit, "OP", mode, "U_FORMAT", "DOUB", "O_FORMAT", oformat,
                      "U_NL", nl, "U_NS", ns, NULL);

   printf("unit: %d status: %d\n", *unit, status);
   assert(status == 1);
}

/******************************************************************************/
VICAR_IMAGE* getInpFile(char *parmName, char *instName, char *fname, int inst)
{
   int unit, cnt, def, status;

   if(parmName != NULL && fname == NULL && strcmp(instName, "inp"))
   {
      status = zvparm(parmName, fname, &cnt, &def, 1, IU_MAX_FNAME_LEN);
      assert(status == 1);
   }
   openFile(&unit, instName, fname, "read", NULL, inst, 0, 0);
   return getImage(unit);
}

/******************************************************************************/
VICAR_IMAGE* getOutFile(char *parmName, char *fmt, char *name, int inst, int nl, int ns)
{
   int unit, cnt, def, status;
   char fname[250];

   status = zvparm(parmName, fname, &cnt, &def, 1, IU_MAX_FNAME_LEN);
   assert(status == 1);
   if(cnt)
   {
      openFile(&unit, name, fname, "write", fmt, inst, nl, ns);
      return getImage(unit);
   }

   return NULL;
}

/******************************************************************************/
ACCA_STRUCT* getACCA()
{
   char fname[IU_MAX_FNAME_LEN];
   int status, cnt, def, i;

   int landsatUnits[ACCA_BANDS_USED];
   int landsatMasterUnits[LANDSAT_N_BANDS];
   int filtNL, filtNS;

   // cloud masks units
   ACCA_STRUCT *acca;
   acca = (ACCA_STRUCT*)malloc(sizeof(ACCA_STRUCT));

   // getting inp files
   for(i = 0; i < ACCA_BANDS_USED; i++)
      openFile(&landsatUnits[i], "inp", "", "read", "NONE", i+1, 0, 0);

   // get LandsatManager
   //   acca->lm = (LANDSAT_MANAGER*)malloc(sizeof(LANDSAT_MANAGER));
   for(i = 0; i < LANDSAT_N_BANDS; i++) landsatMasterUnits[i] = -1;
   landsatMasterUnits[LANDSAT_BAND2] = landsatUnits[0];
   landsatMasterUnits[LANDSAT_BAND3] = landsatUnits[1];
   landsatMasterUnits[LANDSAT_BAND4] = landsatUnits[2];
   landsatMasterUnits[LANDSAT_BAND5] = landsatUnits[3];
   landsatMasterUnits[LANDSAT_BAND61] = landsatUnits[4];

   // getting meta file
   status = zvparm("META", fname, &cnt, &def, 2, IU_MAX_FNAME_LEN);
   assert(status == 1);
   acca->metafile = fopen(fname, "r");
   assert(acca->metafile != 0x0);

   acca->lm = LM_getLandsatManager(landsatMasterUnits, acca->metafile);

   // getting radiance files
   acca->rad2Image = getOutFile("RAD2OUT", "REAL", "rad", 1,
                                 acca->lm->images[LANDSAT_BAND2]->nl, 
                                 acca->lm->images[LANDSAT_BAND2]->ns);
   acca->rad3Image = getOutFile("RAD3OUT", "REAL", "rad", 2,
                                 acca->lm->images[LANDSAT_BAND3]->nl,
                                 acca->lm->images[LANDSAT_BAND3]->ns);
   acca->rad4Image = getOutFile("RAD4OUT", "REAL", "rad", 3,
                                 acca->lm->images[LANDSAT_BAND4]->nl,
                                 acca->lm->images[LANDSAT_BAND4]->ns);
   acca->rad5Image = getOutFile("RAD5OUT", "REAL", "rad", 4,
                                 acca->lm->images[LANDSAT_BAND5]->nl,
                                 acca->lm->images[LANDSAT_BAND5]->ns);
   acca->rad61Image = getOutFile("RAD61OUT", "REAL", "rad", 5,
                                 acca->lm->images[LANDSAT_BAND61]->nl,
                                 acca->lm->images[LANDSAT_BAND61]->ns);

   // getting reflectance files
   acca->ref2Image = getOutFile("REF2OUT", "REAL", "ref", 2,
                                 acca->lm->images[LANDSAT_BAND2]->nl,
                                 acca->lm->images[LANDSAT_BAND2]->ns);
   acca->ref3Image = getOutFile("REF3OUT", "REAL", "ref", 3,
                                 acca->lm->images[LANDSAT_BAND3]->nl,
                                 acca->lm->images[LANDSAT_BAND3]->ns);
   acca->ref4Image = getOutFile("REF4OUT", "REAL", "ref", 4,
                                 acca->lm->images[LANDSAT_BAND4]->nl,
                                 acca->lm->images[LANDSAT_BAND4]->ns);
   acca->ref5Image = getOutFile("REF5OUT", "REAL", "ref", 5,
                                 acca->lm->images[LANDSAT_BAND5]->nl,
                                 acca->lm->images[LANDSAT_BAND5]->ns);

   // getting brightness temperature files
   acca->bTemp61Image = getOutFile("BTEMP61OUT", "REAL", "btemp", 1,
                                    acca->lm->images[LANDSAT_BAND61]->nl,
                                    acca->lm->images[LANDSAT_BAND61]->ns);

   filtNL = acca->lm->images[LANDSAT_BAND61]->nl;
   filtNS = acca->lm->images[LANDSAT_BAND61]->ns;

   // getting output file
   openMaskFile(&(acca->CMcloudUnit), "out", filtNL, filtNS, 1);
   openMaskFile(&(acca->CMsnowUnit), "MASKSNOW", filtNL, filtNS, 2);
   openMaskFile(&(acca->CMdesertUnit), "MASKDESERT", filtNL, filtNS, 3);
   openMaskFile(&(acca->CMcloud_warmUnit), "MASKWARMCLOUD", filtNL, filtNS, 4);
   openMaskFile(&(acca->CMcloud_coldUnit), "MASKCOLDCLOUD", filtNL, filtNS, 5);
   openMaskFile(&(acca->iceUnit), "MASKICE", filtNL, filtNS, 6);
   openMaskFile(&(acca->filter_cirrusUnit), "MASKCIRRUS", filtNL, filtNS, 7);
   openMaskFile(&(acca->ambigUnit), "MASKAMBIG", filtNL, filtNS, 8);
   openMaskFile(&(acca->validUnit), "MASKVALID", filtNL, filtNS, 9);

   openMaskFile(&(acca->filter1Unit), "FILTER1OUT", filtNL, filtNS, 10);
   openMaskFile(&(acca->filter2Unit), "FILTER2OUT", filtNL, filtNS, 11);
   openMaskFile(&(acca->filter3Unit), "FILTER3OUT", filtNL, filtNS, 12);
   openMaskFile(&(acca->filter4Unit), "FILTER4OUT", filtNL, filtNS, 61);
   openMaskFile(&(acca->filter5Unit), "FILTER5OUT", filtNL, filtNS, 14);
   openMaskFile(&(acca->filter6Unit), "FILTER6OUT", filtNL, filtNS, 15);
   openMaskFile(&(acca->filter7Unit), "FILTER7OUT", filtNL, filtNS, 16);
   openMaskFile(&(acca->filter8Unit), "FILTER8OUT", filtNL, filtNS, 17);

   // getting downsampled reflectance files
   acca->ref2dsImage = getVRI(acca->ref2Image, getOutFile("REF2DSOUT", "REAL", "refds", 2, filtNL, filtNS), IU_BILINEAR_INTERP);
   acca->ref3dsImage = getVRI(acca->ref3Image, getOutFile("REF3DSOUT", "REAL", "refds", 3, filtNL, filtNS), IU_BILINEAR_INTERP);
   acca->ref4dsImage = getVRI(acca->ref4Image, getOutFile("REF4DSOUT", "REAL", "refds", 4, filtNL, filtNS), IU_BILINEAR_INTERP);
   acca->ref5dsImage = getVRI(acca->ref5Image, getOutFile("REF5DSOUT", "REAL", "refds", 5, filtNL, filtNS), IU_BILINEAR_INTERP);

   // getting tempcomp file
   acca->tempCompImage = getOutFile("TEMPCOMPOUT", "REAL", "tempcomp", 1, filtNL, filtNS);

   // getting ndsi file
   acca->ndsiImage = getOutFile("NDSIOUT", "REAL", "ndsi", 1, filtNL, filtNS);

   // getting growing vegetation file
   acca->gvImage = getOutFile("GVOUT", "REAL", "gv", 1, filtNL, filtNS);

   // getting senescing vegetation file
   acca->svImage = getOutFile("SVOUT", "REAL", "sv", 1, filtNL, filtNS);

   // getting reflective soil file
   acca->rsImage = getOutFile("RSOUT", "REAL", "rs", 1, filtNL, filtNS);

   return acca;
}

/******************************************************************************/
void deleteACCA(ACCA_STRUCT *acca)
{
   fclose(acca->metafile);

   if(acca->CMcloudUnit != -1) closeMaskFile(acca->CMcloudUnit);
   if(acca->CMsnowUnit != -1) closeMaskFile(acca->CMsnowUnit);
   if(acca->CMdesertUnit != -1) closeMaskFile(acca->CMdesertUnit);
   if(acca->CMcloud_warmUnit != -1) closeMaskFile(acca->CMcloud_warmUnit);
   if(acca->CMcloud_coldUnit != -1) closeMaskFile(acca->CMcloud_coldUnit);
   if(acca->iceUnit != -1) closeMaskFile(acca->iceUnit);
   if(acca->filter_cirrusUnit != -1) closeMaskFile(acca->filter_cirrusUnit);
   if(acca->ambigUnit != -1) closeMaskFile(acca->ambigUnit);
   if(acca->validUnit != -1) closeMaskFile(acca->validUnit);

   if(acca->filter1Unit != -1) closeMaskFile(acca->filter1Unit);
   if(acca->filter2Unit != -1) closeMaskFile(acca->filter2Unit);
   if(acca->filter3Unit != -1) closeMaskFile(acca->filter3Unit);
   if(acca->filter4Unit != -1) closeMaskFile(acca->filter4Unit);
   if(acca->filter5Unit != -1) closeMaskFile(acca->filter5Unit);
   if(acca->filter6Unit != -1) closeMaskFile(acca->filter6Unit);
   if(acca->filter7Unit != -1) closeMaskFile(acca->filter7Unit);
   if(acca->filter8Unit != -1) closeMaskFile(acca->filter8Unit);

   if(acca->rad2Image != NULL) deleteAndCloseImage(&(acca->rad2Image));
   if(acca->rad3Image != NULL) deleteAndCloseImage(&(acca->rad3Image));
   if(acca->rad4Image != NULL) deleteAndCloseImage(&(acca->rad4Image));
   if(acca->rad5Image != NULL) deleteAndCloseImage(&(acca->rad5Image));
   if(acca->rad61Image != NULL) deleteAndCloseImage(&(acca->rad61Image));

   if(acca->ref2Image != NULL) deleteAndCloseImage(&(acca->ref2Image));
   if(acca->ref3Image != NULL) deleteAndCloseImage(&(acca->ref3Image));
   if(acca->ref4Image != NULL) deleteAndCloseImage(&(acca->ref4Image));
   if(acca->ref5Image != NULL) deleteAndCloseImage(&(acca->ref5Image));
   if(acca->bTemp61Image != NULL) deleteAndCloseImage(&(acca->bTemp61Image));
   
   if(acca->tempCompImage != NULL) deleteAndCloseImage(&(acca->tempCompImage));
   if(acca->ndsiImage != NULL) deleteAndCloseImage(&(acca->ndsiImage));
   if(acca->gvImage != NULL) deleteAndCloseImage(&(acca->gvImage));
   if(acca->svImage != NULL) deleteAndCloseImage(&(acca->svImage));
   if(acca->rsImage != NULL) deleteAndCloseImage(&(acca->rsImage));

   if(acca->ref2dsImage->to != NULL) deleteAndCloseImage(&(acca->ref2dsImage->to));
   if(acca->ref3dsImage->to != NULL) deleteAndCloseImage(&(acca->ref3dsImage->to));
   if(acca->ref4dsImage->to != NULL) deleteAndCloseImage(&(acca->ref4dsImage->to));
   if(acca->ref5dsImage->to != NULL) deleteAndCloseImage(&(acca->ref5dsImage->to));

   if(acca->ref2dsImage != NULL) deleteVRI(&(acca->ref2dsImage));
   if(acca->ref3dsImage != NULL) deleteVRI(&(acca->ref3dsImage));
   if(acca->ref4dsImage != NULL) deleteVRI(&(acca->ref4dsImage));
   if(acca->ref5dsImage != NULL) deleteVRI(&(acca->ref5dsImage));

   LM_deleteLandsatManager(&(acca->lm));

   free(acca);
}

/******************************************************************************/
void createRadianceFiles(ACCA_STRUCT *acca)
{
   if(acca->rad2Image != NULL) LM_createRadianceImage(acca->lm, acca->rad2Image, LANDSAT_BAND2);
   if(acca->rad3Image != NULL) LM_createRadianceImage(acca->lm, acca->rad3Image, LANDSAT_BAND3);
   if(acca->rad4Image != NULL) LM_createRadianceImage(acca->lm, acca->rad4Image, LANDSAT_BAND4);
   if(acca->rad5Image != NULL) LM_createRadianceImage(acca->lm, acca->rad5Image, LANDSAT_BAND5);
   if(acca->rad61Image != NULL) LM_createRadianceImage(acca->lm, acca->rad61Image, LANDSAT_BAND61);
}

/******************************************************************************/
void createReflectanceFiles(ACCA_STRUCT *acca)
{
   if(acca->ref2Image != NULL) LM_createReflectanceImage(acca->lm, acca->ref2Image, LANDSAT_BAND2);
   if(acca->ref3Image != NULL) LM_createReflectanceImage(acca->lm, acca->ref3Image, LANDSAT_BAND3);
   if(acca->ref4Image != NULL) LM_createReflectanceImage(acca->lm, acca->ref4Image, LANDSAT_BAND4);
   if(acca->ref5Image != NULL) LM_createReflectanceImage(acca->lm, acca->ref5Image, LANDSAT_BAND5);
}

/******************************************************************************/
void reopenReflectanceFiles(ACCA_STRUCT *acca)
{
   char fname2[IU_MAX_FNAME_LEN], fname3[IU_MAX_FNAME_LEN],
        fname4[IU_MAX_FNAME_LEN], fname5[IU_MAX_FNAME_LEN];

   strcpy(fname2, acca->ref2Image->fname);
   strcpy(fname3, acca->ref3Image->fname);
   strcpy(fname4, acca->ref4Image->fname);
   strcpy(fname5, acca->ref5Image->fname);

   if(acca->ref2Image != NULL) deleteAndCloseImage(&(acca->ref2Image));
   if(acca->ref3Image != NULL) deleteAndCloseImage(&(acca->ref3Image));
   if(acca->ref4Image != NULL) deleteAndCloseImage(&(acca->ref4Image));
   if(acca->ref5Image != NULL) deleteAndCloseImage(&(acca->ref5Image));

   acca->ref2Image = getInpFile(NULL, "NONE", fname2, 2);
   acca->ref3Image = getInpFile(NULL, "NONE", fname3, 3);
   acca->ref4Image = getInpFile(NULL, "NONE", fname4, 4);
   acca->ref5Image = getInpFile(NULL, "NONE", fname5, 5);
}

/******************************************************************************/
void reopenFilesForPass1(ACCA_STRUCT *acca)
{
   char ref2fname[IU_MAX_FNAME_LEN], ref3fname[IU_MAX_FNAME_LEN],
        ref4fname[IU_MAX_FNAME_LEN], ref5fname[IU_MAX_FNAME_LEN];
   char bTemp61fname[IU_MAX_FNAME_LEN];

   strcpy(ref2fname, acca->ref2dsImage->to->fname);
   strcpy(ref3fname, acca->ref3dsImage->to->fname);
   strcpy(ref4fname, acca->ref4dsImage->to->fname);
   strcpy(ref5fname, acca->ref5dsImage->to->fname);

   strcpy(bTemp61fname, acca->bTemp61Image->fname);

   deleteAndCloseImage(&(acca->ref2dsImage->to));
   deleteAndCloseImage(&(acca->ref3dsImage->to));
   deleteAndCloseImage(&(acca->ref4dsImage->to));
   deleteAndCloseImage(&(acca->ref5dsImage->to));
   deleteAndCloseImage(&(acca->bTemp61Image));
   deleteAndCloseImage(&(acca->ref2Image));
   deleteAndCloseImage(&(acca->ref3Image));
   deleteAndCloseImage(&(acca->ref4Image));
   deleteAndCloseImage(&(acca->ref5Image));

   acca->ref2Image = getInpFile(NULL, "ref", ref2fname, 2);
   acca->ref3Image = getInpFile(NULL, "ref", ref3fname, 3);
   acca->ref4Image = getInpFile(NULL, "ref", ref4fname, 4);
   acca->ref5Image = getInpFile(NULL, "ref", ref5fname, 5);

   acca->bTemp61Image = getInpFile(NULL, "btemp", bTemp61fname, 1);
}

/******************************************************************************/
void createDownSampledFiles(ACCA_STRUCT *acca)
{
   if(acca->ref2dsImage != NULL) acca->ref2dsImage->from = acca->ref2Image;
   if(acca->ref3dsImage != NULL) acca->ref3dsImage->from = acca->ref3Image;
   if(acca->ref4dsImage != NULL) acca->ref4dsImage->from = acca->ref4Image;
   if(acca->ref5dsImage != NULL) acca->ref5dsImage->from = acca->ref5Image;

   if(acca->ref2dsImage != NULL) createDownSampledImage(acca->ref2dsImage);
   if(acca->ref3dsImage != NULL) createDownSampledImage(acca->ref3dsImage);
   if(acca->ref4dsImage != NULL) createDownSampledImage(acca->ref4dsImage);
   if(acca->ref5dsImage != NULL) createDownSampledImage(acca->ref5dsImage);
}

/******************************************************************************/
void createBTempFiles(ACCA_STRUCT *acca)
{
   if(acca->bTemp61Image) LM_createBTempImage(acca->lm, acca->bTemp61Image, LANDSAT_BAND61);
}

/******************************************************************************/
void createMaskFile(ACCA_STRUCT *acca, CLOUD_MASKS *cm, int type)
{
   int i, status;

   for(i = 0; i < cm->vars->nl; i++)
   {
      switch(type)
      {
         case CM_SNOWMASK:
            if(acca->CMsnowUnit == -1) return;
            status = zvwrit(acca->CMsnowUnit, cm->CMsnow[i], "LINE", i+1, NULL);
            assert(status == 1);
            break;
         case CM_VALIDMASK:
            if(acca->validUnit == -1) return;
            status = zvwrit(acca->validUnit, cm->valid[i], "LINE", i+1, NULL);
            assert(status == 1);
            break;
         case CM_AMBIGMASK:
            if(acca->ambigUnit == -1) return;
            status = zvwrit(acca->ambigUnit, cm->ambig[i], "LINE", i+1, NULL);
            assert(status == 1);
            break;
         case CM_CLOUDMASK:
            if(acca->CMcloudUnit == -1) return;
            status = zvwrit(acca->CMcloudUnit, cm->CMcloud[i], "LINE", i+1, NULL);
            assert(status == 1);
            break;
         case CM_WARMCLOUDMASK:
            if(acca->CMcloud_warmUnit == -1) return;
            status = zvwrit(acca->CMcloud_warmUnit, cm->CMcloud_warm[i], "LINE", i+1, NULL);
            assert(status == 1);
            break;
         case CM_COLDCLOUDMASK:
            if(acca->CMcloud_coldUnit == -1) return;
            status = zvwrit(acca->CMcloud_coldUnit, cm->CMcloud_cold[i], "LINE", i+1, NULL);
            assert(status == 1);
            break;

         case CM_FILTER1:
            if(acca->filter1Unit == -1) return;
            status = zvwrit(acca->filter1Unit, cm->filter1[i], "LINE", i+1, NULL);
            assert(status == 1);
            break;
         case CM_FILTER2:
            if(acca->filter2Unit == -1) return;
            status = zvwrit(acca->filter2Unit, cm->filter2[i], "LINE", i+1, NULL);
            assert(status == 1);
            break;
         case CM_FILTER3:
            if(acca->filter3Unit == -1) return;
            status = zvwrit(acca->filter3Unit, cm->filter3[i], "LINE", i+1, NULL);
            assert(status == 1);
            break;
         case CM_FILTER4:
            if(acca->filter4Unit == -1) return;
            status = zvwrit(acca->filter4Unit, cm->filter4[i], "LINE", i+1, NULL);
            assert(status == 1);
            break;
         case CM_FILTER5:
            if(acca->filter5Unit == -1) return;
            status = zvwrit(acca->filter5Unit, cm->filter5[i], "LINE", i+1, NULL);
            assert(status == 1);
            break;
         case CM_FILTER6:
            if(acca->filter6Unit == -1) return;
            status = zvwrit(acca->filter6Unit, cm->filter6[i], "LINE", i+1, NULL);
            assert(status == 1);
            break;
         case CM_FILTER7:
            if(acca->filter7Unit == -1) return;
            status = zvwrit(acca->filter7Unit, cm->filter7[i], "LINE", i+1, NULL);
            assert(status == 1);
            break;
         case CM_FILTER8:
            if(acca->filter8Unit == -1) return;
            status = zvwrit(acca->filter8Unit, cm->filter8[i], "LINE", i+1, NULL);
            assert(status == 1);
            break;

         assert(0);
      }
   }
}

/******************************************************************************/
void createMaskFiles(ACCA_STRUCT *acca, CLOUD_MASKS *cm)
{ 
   if(acca->CMsnowUnit != -1) createMaskFile(acca, cm, CM_SNOWMASK);
   if(acca->validUnit != -1) createMaskFile(acca, cm, CM_VALIDMASK);
   if(acca->ambigUnit != -1) createMaskFile(acca, cm, CM_AMBIGMASK);
   if(acca->CMcloudUnit != -1) createMaskFile(acca, cm, CM_CLOUDMASK);
   if(acca->CMcloud_warmUnit != -1) createMaskFile(acca, cm, CM_WARMCLOUDMASK);
   if(acca->CMcloud_coldUnit != -1) createMaskFile(acca, cm, CM_COLDCLOUDMASK);

   if(acca->filter1Unit != -1) createMaskFile(acca, cm, CM_FILTER1);
   if(acca->filter2Unit != -1) createMaskFile(acca, cm, CM_FILTER2);
   if(acca->filter3Unit != -1) createMaskFile(acca, cm, CM_FILTER3);
   if(acca->filter4Unit != -1) createMaskFile(acca, cm, CM_FILTER4);
   if(acca->filter5Unit != -1) createMaskFile(acca, cm, CM_FILTER5);
   if(acca->filter6Unit != -1) createMaskFile(acca, cm, CM_FILTER6);
   if(acca->filter7Unit != -1) createMaskFile(acca, cm, CM_FILTER7);
   if(acca->filter8Unit != -1) createMaskFile(acca, cm, CM_FILTER8);
}
