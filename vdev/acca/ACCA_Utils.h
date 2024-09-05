#ifndef ACCA_UTILS
#define ACCA_UTILS

#define ACCA_BANDS_USED  5
#define ACCA_REF_UNITS   4

#include "cloud_masks.h"
#include "LandsatManager.h"

typedef struct
{
   FILE *metafile;

   LANDSAT_MANAGER *lm;

   int CMcloudUnit;
   int CMsnowUnit;
   int CMdesertUnit;
   int CMcloud_warmUnit;
   int CMcloud_coldUnit;
   int iceUnit;
   int filter_cirrusUnit;
   int ambigUnit;
   int validUnit;

   int filter1Unit;
   int filter2Unit;
   int filter3Unit;
   int filter4Unit;
   int filter5Unit;
   int filter6Unit;
   int filter7Unit;
   int filter8Unit;

   VICAR_IMAGE *rad2Image;
   VICAR_IMAGE *rad3Image;
   VICAR_IMAGE *rad4Image;
   VICAR_IMAGE *rad5Image;
   VICAR_IMAGE *rad61Image;

   VICAR_IMAGE *ref2Image;
   VICAR_IMAGE *ref3Image;
   VICAR_IMAGE *ref4Image;
   VICAR_IMAGE *ref5Image;
   VICAR_IMAGE *bTemp61Image;

   VICAR_RESAMPLE_IMAGE *ref2dsImage;
   VICAR_RESAMPLE_IMAGE *ref3dsImage;
   VICAR_RESAMPLE_IMAGE *ref4dsImage;
   VICAR_RESAMPLE_IMAGE *ref5dsImage;

   VICAR_IMAGE *tempCompImage;
   VICAR_IMAGE *ndsiImage;
   VICAR_IMAGE *gvImage;
   VICAR_IMAGE *svImage;
   VICAR_IMAGE *rsImage;
}ACCA_STRUCT;

/******************************************************************************/
// getACCA: returns an initialized ACCA struct
//
// output:
// =======
// + ACCA_STRUCT pointer
/******************************************************************************/
ACCA_STRUCT* getACCA();

/******************************************************************************/
// deleteACCA: deletes a given ACCA struct
//              frees buffers and closes all files
//
// input:
// ======
// + *acca
//    - ACCA_STRUCT to delete
/******************************************************************************/
void deleteACCA(ACCA_STRUCT *acca);

/******************************************************************************/
// createRadianceFiles: creates radiance files for all raw dn files
//
// input:
// ======
// + *acca
//    - ACCA_STRUCT to containing raw dn images
/******************************************************************************/
void createRadianceFiles(ACCA_STRUCT *acca);

/******************************************************************************/
// createReflectanceFiles: creates reflectance files for VNIR/SWIR raw dn files
//
// input:
// ======
// + *acca
//    - ACCA_STRUCT to containing raw dn images
/******************************************************************************/
void createReflectanceFiles(ACCA_STRUCT *acca);

/******************************************************************************/
// createBTempFiles: creates brightness temperature files for TIR raw dn files
//
// input:
// ======
// + *acca
//    - ACCA_STRUCT to containing raw dn images
/******************************************************************************/
void createBTempFiles(ACCA_STRUCT *acca);

/******************************************************************************/
// createDownsampledFiles: creates downsampled files for reflectance files
//
// input:
// ======
// + *acca
//    - ACCA_STRUCT to containing reflectance files
/******************************************************************************/
void createDownSampledFiles(ACCA_STRUCT *acca);

/******************************************************************************/
// reopenReflectanceFiles: reopens reflectance files for reading
//
// input:
// ======
// + *acca
//    - ACCA_STRUCT to containing reflectance files
/******************************************************************************/
void reopenReflectanceFiles(ACCA_STRUCT *acca);

/******************************************************************************/
// reopenFilesForPass1: changes reflectance file pointers to downsampled
//                      reflectance files
//
// input:
// ======
// + *acca
//    - ACCA_STRUCT to containing reflectance files
/******************************************************************************/
void reopenFilesForPass1(ACCA_STRUCT *acca);

/******************************************************************************/
// createMaskFiles: creates mask files from 
//
// input:
// ======
// + *acca
//    - ACCA_STRUCT to containing mask file units
//
// output:
// =======
// + *cm
//    - CLOUD_MASKS struct containing binary masks
/******************************************************************************/
void createMaskFiles(ACCA_STRUCT *acca, CLOUD_MASKS *cm);

#endif
