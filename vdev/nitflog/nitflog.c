#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "vicmain_c.h"
#include "zvproto.h"

#include "carto/cartoLoggerUtils.h"
#include "carto/cartoVicarProtos.h"

#include "nitftools.h"

#define _nitflog_version_ "Sat Dec 29 2007"

void main44(void)
{
  int vunit;
  FILE * nitfFile;
  int parmct, parmdf;
  char infilename [99];
  char msgBuf [100];
  NITFFileHeader header;
  int imageCount, imageIndex;
  char outname [99];
  int echoMeta = 0;
  int status;
  int noImg;

  checkLoggerUtilsVersion (15);

  sprintf (msgBuf, "nitflog version %s", _nitflog_version_);
  zifmessage (msgBuf);
   
  /* fetch params */
  zvparm ("inp", infilename, & parmct, & parmdf, 1, 99);
  zvparm ("out", outname, & parmct, & parmdf, 1, 99);
  echoMeta = zvptst ("echoMeta");
  noImg = zvptst ("noImg");

  if (! (nitfFile = fopen (infilename, "r"))) {
    sprintf (msgBuf, "error opening %s for input", infilename);
    zmabend (msgBuf);
  }

  /* parse the header */
  if ((status = NITF_parseHeader (nitfFile, & header))) {
    sprintf (msgBuf, "error parsing NITF header: code %d", status);
    zmabend (msgBuf);
  }

  imageCount = header . NUMI;

  /* for each image */
  for (imageIndex = 0; imageIndex < imageCount; imageIndex ++) {
    int line;
    int nl = header . images [imageIndex] . NROWS;
    int ns = header . images [imageIndex] . NCOLS;
    int lineSize = NITF_paddedImageLineSizeInBytes (& header, imageIndex);
    char * lineBuf = malloc (lineSize);
    int bytesPerPixel = header . images [imageIndex] . NBPP == 8 ? 1 : 2;
    char outImageName [300];
    char outMetaName [300];

    if (! lineBuf)
      zmabend ("lineBuf allocation failed");

    /* generate outfile names */
    if (! strlen (outname)) {
      sprintf (outImageName, "%s_%s_%d.img", infilename, header . images [imageIndex] . IDATIM, imageIndex);
      sprintf (outMetaName, "%s_%s_%d.txt", infilename, header . images [imageIndex] . IDATIM, imageIndex);
    } else {
      if (imageCount > 1) {
	sprintf (outImageName, "%s_%d.img", outname, imageIndex);
	sprintf (outMetaName, "%s_%d.txt", outname, imageIndex);
      } else {
	sprintf (outImageName, "%s.img", outname);
	sprintf (outMetaName, "%s.txt", outname);
      }
    }

    initMetaData (outMetaName);

    if (! noImg) {
      /* open VICAR image output */
      if (zvunit (& vunit, "U_NAME", 1, "U_NAME", outImageName, NULL) != 1)
	zmabend ("zvunit failed for out image");

      if (bytesPerPixel == 1) {
	if (zvopen (vunit, "U_NL", nl, "U_NS", ns, "OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA", "O_FORMAT", "BYTE", NULL) != 1)
	  zmabend ("zvopen failed");
      } else
	if (zvopen (vunit, "U_NL", nl, "U_NS", ns, "OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA", "O_FORMAT", "HALF", NULL) != 1)
	  zmabend ("zvopen failed");
    }

    /* log some meta data */
    logMetaString (echoMeta, outMetaName, "NITFLOG_VERSION", _nitflog_version_, ! noImg, & vunit);
    logMetaString (echoMeta, outMetaName, "LOG_INP", infilename, ! noImg, & vunit);
    logMetaString (echoMeta, outMetaName, "NITF_VERSION", header . FHDR, ! noImg, & vunit);
    logMetaString (echoMeta, outMetaName, "NITF_FILE_DATE_TIME", header . FDT, ! noImg, & vunit);
    logMetaString (echoMeta, outMetaName, "NITF_FILE_TITLE", header . FTITLE, ! noImg, & vunit);
#ifndef USE_MUSE_CODE_AS_SPEC
    logMetaString (echoMeta, outMetaName, "NITF_ORIGINATORS_NAME", header . ONAME, ! noImg, & vunit);
#endif
    logMetaInt (echoMeta, outMetaName, "NITF_NUMBER_OF_IMAGES", header . NUMI, ! noImg, & vunit);
    logMetaString (echoMeta, outMetaName, "NITF_IMAGE_ID", header . images [imageIndex] . IID1, ! noImg, & vunit);
    logMetaString (echoMeta, outMetaName, "NITF_IMAGE_TITLE", header . images [imageIndex] . IID2, ! noImg, & vunit);
    logMetaString (echoMeta, outMetaName, "NITF_IMAGE_DATE_TIME", header . images [imageIndex] . IDATIM, ! noImg, & vunit);
    logMetaString (echoMeta, outMetaName, "NITF_IMAGE_SOURCE", header . images [imageIndex] . ISORCE, ! noImg, & vunit);
    logMetaInt (echoMeta, outMetaName, "ARCH_NL", header . images [imageIndex] . NROWS, ! noImg, & vunit);
    logMetaInt (echoMeta, outMetaName, "ARCH_NS", header . images [imageIndex] . NCOLS, ! noImg, & vunit);
    logMetaString (echoMeta, outMetaName, "NITF_BITS_PER_PIXEL", header . images [imageIndex] . ABPP, ! noImg, & vunit);
    logMetaDouble (echoMeta, outMetaName, "NITF_IGEOLO_ULLAT", header . images [imageIndex] . ULLat, ! noImg, & vunit);
    logMetaDouble (echoMeta, outMetaName, "NITF_IGEOLO_ULLON", header . images [imageIndex] . ULLon, ! noImg, & vunit);
    logMetaDouble (echoMeta, outMetaName, "NITF_IGEOLO_URLAT", header . images [imageIndex] . URLat, ! noImg, & vunit);
    logMetaDouble (echoMeta, outMetaName, "NITF_IGEOLO_URLON", header . images [imageIndex] . URLon, ! noImg, & vunit);
    logMetaDouble (echoMeta, outMetaName, "NITF_IGEOLO_LRLAT", header . images [imageIndex] . LRLat, ! noImg, & vunit);
    logMetaDouble (echoMeta, outMetaName, "NITF_IGEOLO_LRLON", header . images [imageIndex] . LRLon, ! noImg, & vunit);
    logMetaDouble (echoMeta, outMetaName, "NITF_IGEOLO_LLLAT", header . images [imageIndex] . LLLat, ! noImg, & vunit);
    logMetaDouble (echoMeta, outMetaName, "NITF_IGEOLO_LLLON", header . images [imageIndex] . LLLon, ! noImg, & vunit);

    if (! noImg) {
      /* read/write the image */
      for (line = 0; line < nl; line ++) {
	if (NITF_extractImageLine (nitfFile, & header, imageIndex, line, lineBuf))
	  zmabend ("error extracting image line");

	zvwrit (vunit, lineBuf, "LINE", line + 1, "SAMP", 1, "NSAMPS", ns, NULL);
      }

      zvclose (vunit, NULL);
      free (lineBuf);
    }
  }

  fclose (nitfFile);
}
