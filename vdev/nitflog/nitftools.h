#ifndef _nitftools_h_
#define _nitftools_h_

#include <stdio.h>

enum {NITFTOOLS_SUCCESS,
      FOPEN_FAILED, FSEEK_FAILED, FREAD_FAILED, FWRITE_FAILED,
      SSCANF_FAILED,
      MALLOC_FAILED, REALLOC_FAILED
} NITFToolsStatus;

/* The NITFFileHeader structure below conforms either to the NITF
   spec, MIL-STD-2500B, or MUSE code designed to read CIB, which is
   supposed to conform to the RPF spec, which is supposed to conform
   to the NITF spec. */

#define USE_MUSE_CODE_AS_SPEC

typedef struct NITFFileHeader {
  char FHDR [10];		/* File Type & Version */
  char CLEVEL [3];		/* Complexity Level */
  char STYPE [5];		/* System Type */
  char OSTAID [11];		/* Originating Station ID */
  char FDT [15];		/* File Date & Time */
  char FTITLE [81];		/* File Title */
  char FSCLAS [2];		/* File Security Classification */
#ifdef USE_MUSE_CODE_AS_SPEC
  char MUSE_FILE_CODE_WORDS [41];
  char MUSE_FILE_CONTROL_AND_HANDLING [41];
  char MUSE_FILE_RELEASE_INSTRUCTIONS [41];
  char MUSE_FILE_CLASSIFICATION_AUTHORITY [21];
  char MUSE_FILE_SECURITY_CONTROL_NUMBER [21];
  char MUSE_FILE_SECURITY_DOWNGRADE [7];
  char MUSE_FILE_SECURITY_DOWNGRADING_EVENT [41];
#else
  char FSCLSY [3];		/* File Classification Security System */
  char FSCODE [12];		/* File Codewords */
  char FSCTLH [3];		/* File Control & Handling */
  char FSREL [21];		/* File Releasing Instructions */
  char FSDCTP [3];		/* File Declassification Type */
  char FSDCDT [9];		/* File Declassification Date */
  char FSDCXM [5];		/* File Declassification Exemption */
  char FSDG [2];		/* File Downgrade */
  char FSDGDT [9];		/* File Downgrade Date */
  char FSCLTX [44];		/* File Classification Text */
  char FSCLTP [2];		/* File Classification Authority Type */
  char FSCAUT [41];		/* File Classification Authority */
  char FSCRSN [2];		/* File Classification Reason */
  char FSSRDT [9];		/* File Security Source Date */
  char FSCTLN [16];		/* File Security Control Number */
#endif
  char FSCOP [6];		/* File Copy Number */
  char FSCPYS [6];		/* File Number Of Copies */
  char ENCRYP [2];		/* Encryption */
#ifdef USE_MUSE_CODE_AS_SPEC
  char MUSE_ORIGINATORS_NAME [28];
  char MUSE_ORIGINATORS_PHONE_NUMBER [19];
#else
  char FBKGC [4];		/* File Background Color */
  char ONAME [25];		/* Originator's Name */
  char OPHONE [19];		/* Originator's Phone Number */
#endif
  char FL [13];			/* File Length */
  int HL;			/* Header Length (6 char field) */
  int NUMI;			/* Number of Images (3 char field) */
  struct NITFImageSubheader * images;
  int NUMS;			/* Number of graphics (3 char field) */
  struct NITFGraphicSubheader * graphics;
#ifdef USE_MUSE_CODE_AS_SPEC
  int MUSE_NUM_LABELS;
#endif
  int NUMT;			/* Number of texts (3 char field) */
  struct NITFTextSubheader * text;
  int NUMDES;			/* Number of data (3 char field) */
  struct NITFDataSubheader * data;
  int NUMRES;			/* Number of reserved extensions (3 char field) */
  struct NITFReservedExtensionSubheader * reservedExtension;
  int UDHDL;			/* Length of user defined subheader (5 char field) */
  struct NITFUserDefinedSubheader * userDefined;
  int XHDL;			/* Length of extended subheader (5 char field) */
  struct NITFExtendedSubheader * extended;
} NITFFileHeader;

typedef struct NITFImageSubheader {
  int LISHnnn;			/* Length of Nth Image Subheader */
  int LInnn;			/* Length of Nth Image */
  char IM [3];			/* File Part Type */
  char IID1 [11];		/* Image ID Part 1 */
  char IDATIM [15];		/* Image Date & Time */
  char TGTID [18];		/* Target ID */
  char IID2 [81];		/* Image ID Part 2 */
  char ISCLAS [2];		/* Image Security Classification */
  char ISCLSY [3];		/* Image Security Classification System */
  char ISCODE [12];		/* Image Codewords */
  char ISCTLH [3];		/* Image Control & Handling */
  char ISREL [21];		/* Image Releasing Instructions */
  char ISDCTP [3];		/* Image Declassification Type */
  char ISDCDT [9];		/* Image Declassification Date */
  char ISDCXM [5];		/* Image Declassification Exemption */
  char ISDG [2];		/* Image Downgrade */
  char ISDGDT [9];		/* Image Downgrade Date */
  char ISCLTX [44];		/* Image Classification Text */
  char ISCATP [2];		/* Image Classification Authority Type */
  char ISCAUT [41];		/* Image Classification Authority */
  char ISCRSN [2];		/* Image Classification Reason */
  char ISSRDT [9];		/* Image Classification Source Date */
  char ISCTLN [16];		/* Image Security Control Number */
  char ENCRYP [2];		/* Encryption */
  char ISORCE [43];		/* Image Source */
  int NROWS;			/* Number of Significant Rows in Image */
  int NCOLS;			/* Number of Significant Columns in Image */
  char PVTYPE [4];		/* Pixel Value Type */
  char IREP [9];		/* Image Representation */
  char ICAT [9];		/* Image Category */
  char ABPP [3];		/* Actual Bits-Per-Pixel Per Band */
  char PJUST [2];		/* Pixel Justification */
  char ICORDS [2];		/* Image Coordinate System */
  double ULLat, ULLon;		/* IGEOLO corner 0 in decimal degrees */
  double URLat, URLon;		/* IGEOLO corner 1 in decimal degrees */
  double LRLat, LRLon;		/* IGEOLO corner 2 in decimal degrees */
  double LLLat, LLLon;		/* IGEOLO corner 3 in decimal degrees */
  int NICOM;			/* Number of Image Comments */
  char ** ICOMn;		/* Image Comments */
  char IC [3];			/* Image Compression */
  char COMRAT [5];		/* Compression Rate Code */
  int NBANDS;			/* Number of Bands */
  int XBANDS;			/* Number of Multi-Spectral Bands */
  struct NITFBand * bands;	/* band data */
  char ISYNC [2];		/* Image Sync Mode */
  char IMODE [2];		/* Image Mode */
  int NBPR;			/* Number of Blocks Per Row */
  int NBPC;			/* Number of Blocks Per Column */
  int NPPBH;			/* Number of Pixels Per Block Horizontal */
  int NPPBV;			/* Number of Pixels Per Block Vertical */
  int NBPP;			/* Number of Bits Per Pixel Per Band */
  char IDLVL [4];		/* Display Level */
  char IALVL [4];		/* Attachment Level */
  char ILOC [11];		/* Image Location */
  char IMAG [5];		/* Image Magnification */
  int UDIDL;			/* User Defined Image Data Length */
  char UDOFL [4];		/* User Defined Overflow */
  char * UDID;			/* User Defined Image Data */
  int IXSHDL;			/* Extended Subheader Data Length */
  char IXSOFL [4];		/* Extended Subheader Overflow */
  char * IXSHD;			/* Extended Subheader Data */
} NITFImageSubheader;

typedef struct NITFGraphicSubheader {
  int LSSHnnn;			/* Length of Nth Graphic Subheader */
  int LSnnn;			/* Length of Nth Graphic */
} NITFGraphicSubheader;

typedef struct NITFTextSubheader {
  int LTSHnnn;			/* Length of Nth Text Subheader */
  int LTnnn;			/* Length of Nth Text */
} NITFTextSubheader;

typedef struct NITFDataSubheader {
  int LDSHnnn;			/* Length of Nth Data Subheader */
  int LDnnn;			/* Length of Nth Data */
} NITFDataSubheader;

typedef struct NITFReservedExtensionSubheader {
  int LRESHnnn;			/* Length of Nth Reserved Extension Subheader */
  int LREnnn;			/* Length of Nth Reserved Extension */
} NITFReservedExtensionSubheader;

typedef struct NITFUserDefinedSubheader {
  char UDHOFL [4];		/* User Defined Header Overflow */
  char * UDHD;			/* User Defined Header Data */
} NITFUserDefinedSubheader;

typedef struct NITFExtendedSubheader {
  char XHDOFL [3];		/* Extended Header Overflow */
  char * XHD;			/* Extended Header Data */
} NITFExtendedSubheader;

typedef struct NITFBand {
  char IREPBANDnn [3];		/* nnth Band Representation */
  char ISUBCATnn [7];		/* nnth Band Subcategory */
  char IFCnn [2];		/* nnth Band Image Filter Condition */
  char IMFLTnn [4];		/* nnth Band Standard Image Filter Code */
  int NLUTSnn;			/* nnth Band Number of LUTS */
  int NELUTnn;			/* nnth Band Number of LUT Entries */
  char ** LUTDnnm;		/* Image LUTs for this band */
} NITFBand;


/* NITF_parseHeader reads an NITF header, including its subheaders,
 * from nitf and stores in header. 
 *
 * Parameters
 * nitf    IN pointer to open NITF file
 * header OUT pointer to existing NITFFileHeader
 *
 * Returns 0 on success, nonzero otherwise */
int NITF_parseHeader (FILE * nitf, NITFFileHeader * header);

/* NITF_extractImageToFile reads an image from nitf and writes it as a
 * single untiled image with no header to the file indicated by
 * outPath. This function does not support compressed or masked
 * images.
 *
 * Parameters
 * nitf       IN FILE pointer to NITF file
 * header     IN pointer to header filled by NITF_parseHeader
 * imageIndex IN Zero based image index
 * outPath   OUT Path of file to hold extracted image */
int NITF_extractImageToFile (FILE * nitf, NITFFileHeader * header, int imageIndex, char * outPath);

int NITF_paddedImageLineSizeInBytes (NITFFileHeader * header,int imageIndex);
int NITF_extractImageLine (FILE * nitf, NITFFileHeader * header, int imageIndex, int line, char * lineBuf);

#endif
