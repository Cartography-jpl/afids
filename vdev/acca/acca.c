#include "vicmain_c.h"
#include "applic.h"

#include "cloud_masks.h"
#include "carto/ImageUtils.h"
#include "LandsatManager.h"
#include "ACCA_Utils.h"

/******************************************************************************/
void main44(void)
{
   ACCA_STRUCT *acca;
   CLOUD_MASKS *masks;

   // preprocess2
   acca = getACCA();

   //   createRadianceFiles(acca);
   createReflectanceFiles(acca);
   reopenReflectanceFiles(acca);
   createBTempFiles(acca);
   createDownSampledFiles(acca);
   reopenFilesForPass1(acca);
   // process pass-1
   masks = get_CLOUD_MASKS(acca->bTemp61Image->nl, acca->bTemp61Image->ns);
   setBandImages(&masks, acca->ref2Image, acca->ref3Image, acca->ref4Image,
                 acca->ref5Image, acca->bTemp61Image);
   setWorkspaceImages(&masks, acca->ndsiImage, acca->tempCompImage,
                      acca->gvImage, acca->svImage, acca->rsImage);
   init_CM_WORKSPACE(&masks);
   doPass1(masks);
   doPass2(masks);

   createMaskFiles(acca, masks);

   deleteACCA(acca);
}
