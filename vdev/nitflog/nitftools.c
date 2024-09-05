#include <stdlib.h>
#include <string.h>

#include "nitftools.h"

/* NITF file header field offsets */
#define NITF_HDR_HL_OFFSET 354

static void copyField (int * offset, int * fieldSize, int thisFieldSize, char * to, char * headerBuf) {
  * offset += * fieldSize;
  * fieldSize = thisFieldSize;
  strncpy (to, headerBuf + * offset, * fieldSize);
  to [* fieldSize] = 0;
}

static int scanACorner (char * buf, int corner, double * lat, double * lon) {
  int deg, min, sec;

  /* example IGEOLO field; note: there are other formats; see MIL-STD-2500B */
  /* ddmmssXdddmmssYddmmssXdddmmssYddmmssXdddmmssYddmmssXdddmmssY */
  /* 382607N0935438W382607N0935332W382517N0935332W382517N0935438W */
  /* 012345678901234567890123456780912345678901234567890123456789 */

  /* an example not supported */
  /* 11SMS849442125711SMS869442125711SMS869441925711SMS8494419257 */
  /* 012345678901234567890123456780912345678901234567890123456789 */

  /* scan lat */
  if (sscanf (buf + corner * 15,     "%02d", & deg) != 1) return 1;
  if (sscanf (buf + corner * 15 + 2, "%02d", & min) != 1) return 1;
  if (sscanf (buf + corner * 15 + 4, "%02d", & sec) != 1) return 1;
  if (buf [corner * 15 + 6] == 'N')
    * lat = deg + min / 60.0 + sec / 3600.0;
  else if (buf [corner * 15 + 6] == 'S')
    * lat = - (deg + min / 60.0 + sec / 3600.0);
  else
    return 1;

  /* scan lon */
  if (sscanf (buf + corner * 15 + 7,  "%03d", & deg) != 1) return 1;
  if (sscanf (buf + corner * 15 + 10, "%02d", & min) != 1) return 1;
  if (sscanf (buf + corner * 15 + 12, "%02d", & sec) != 1) return 1;
  if (buf [corner * 15 + 14] == 'E')
    * lon = deg + min / 60.0 + sec / 3600.0;
  else if (buf [corner * 15 + 14] == 'W')
    * lon = - (deg + min / 60.0 + sec / 3600.0);
  else
    return 1;

  return 0;
}

int MUSE_getNITFHeaderLength (FILE *infp)
{
  long intvar;
  char string[128];

  /* read in file type and version */
  fread(string, 1, 9, infp);
  string[9] = '\0';
  
  /* read in compliance level */
  fread(string, 1, 2, infp);
  string[2] = '\0';

  /* read in system type */
  fread(string, 1, 4, infp);
  string[4] = '\0';
  
  /* read in originating station id */
  fread(string, 1, 10, infp);
  string[10] = '\0';

  /* read in file date and time stamp */
  fread(string, 1, 14, infp);
  string[14] = '\0';
  
  /* read in file title */
  fread(string, 1, 80, infp);
  string[80] = '\0';

  /* read in security classification */
  fread(string, 1, 1, infp);
  string[1] = '\0';
  
  /* read in file code words */
  fread(string, 1, 40, infp);
  string[40] = '\0';
  
  /* read in file control and handling */
  fread(string, 1, 40, infp);
  string[40] = '\0';

  /* read in file release instructions */
  fread(string, 1, 40, infp);
  string[40] = '\0';
  
  /* read in file classification authority */
  fread(string, 1, 20, infp);
  string[20] = '\0';

  /* read in file security control number */
  fread(string, 1, 20, infp);
  string[20] = '\0';

  /* read in file security downgrade */
  fread(string, 1, 6, infp);
  string[6] = '\0';
  
  intvar = atoi(string);
  if(intvar == 999998) {
    /* read in file security downgrading event */
    fread(string, 1, 40, infp);
    string[40] = '\0';
  }

  /* read in message copy number */
  fread(string, 1, 5, infp);
  string[5] = '\0';
  
  /* read in number of copies */
  fread(string, 1, 5, infp);
  string[5] = '\0';
  
  /* read in encryption */
  fread(string, 1, 1, infp);
  string[1] = '\0';
  
  /* read in originator's name */
  fread(string, 1, 27, infp);
  string[27] = '\0';
  
  /* read in originator's phone number */
  fread(string, 1, 18, infp);
  string[18] = '\0';

  /* read in file length */
  fread(string, 1, 12, infp);
  string[12] = '\0';

  /* read in NITF file header length */
  fread(string, 1, 6, infp);
  string[6] = '\0';

  {
    int headerLength;
    if (sscanf (string, "%d", & headerLength) != 1)
      return -1;
    else
      return headerLength;
  }
}

int NITF_parseHeader (FILE * nitf, NITFFileHeader * header) {
  char * headerBuf, * headerFieldBuf;
  int fieldSize, offset;

  /* get length of file header */
#ifdef USE_MUSE_CODE_AS_SPEC
  header -> HL = MUSE_getNITFHeaderLength (nitf);
#else
  if (fseek (nitf, NITF_HDR_HL_OFFSET, SEEK_SET)) return FSEEK_FAILED;
  if (fread (buf, 6, 1, nitf) != 1) return FREAD_FAILED;
  buf [6] = 0;
  if (sscanf (buf, "%d", & header -> HL) != 1) return SSCANF_FAILED;
#endif

  printf ("Header Length: %d\n", header -> HL);

  /* malloc space and read the whole file header */
  printf ("mallocing %d byte headerBuf\n", header -> HL);
  if (! (headerBuf = (char *) malloc (header -> HL))) return MALLOC_FAILED;
  if (! (headerFieldBuf = (char *) malloc (header -> HL))) return MALLOC_FAILED;
  if (fseek (nitf, 0, SEEK_SET)) return FSEEK_FAILED;
  if (fread (headerBuf, header -> HL, 1, nitf) != 1) return FREAD_FAILED;

  offset = fieldSize = 0;

  /* File Type & Version */
  copyField (& offset, & fieldSize, 9, header -> FHDR, headerBuf);
  printf ("File Type & Version: %s\n", header -> FHDR);
  /* Complexity Level */
  copyField (& offset, & fieldSize, 2, header -> CLEVEL, headerBuf);
  printf ("Complexity Level: %s\n", header -> CLEVEL);
  /* System Type */
  copyField (& offset, & fieldSize, 4, header -> STYPE, headerBuf);
  printf ("System Type: %s\n", header -> STYPE);
  /* Originating Station ID */
  copyField (& offset, & fieldSize, 10, header -> OSTAID, headerBuf);
  printf ("Originating Station ID: %s\n", header -> OSTAID);
  /* File Date & Time */
  copyField (& offset, & fieldSize, 14, header -> FDT, headerBuf);
  printf ("File Date & Time: %s\n", header -> FDT);
  /* File Title */                          
  copyField (& offset, & fieldSize, 80, header -> FTITLE, headerBuf);
  printf ("File Title: %s\n", header -> FTITLE);
  /* File Security Classification */        
  copyField (& offset, & fieldSize, 1, header -> FSCLAS, headerBuf);
  printf ("File Security Classification: %s\n", header -> FSCLAS);

#ifdef USE_MUSE_CODE_AS_SPEC
  /* MUSE_FILE_CODE_WORDS */
  copyField (& offset, & fieldSize, 40, header -> MUSE_FILE_CODE_WORDS, headerBuf);
  printf ("MUSE_FILE_CODE_WORDS: %s\n", header -> MUSE_FILE_CODE_WORDS);
  /* MUSE_FILE_CONTROL_AND_HANDLING */
  copyField (& offset, & fieldSize, 40, header -> MUSE_FILE_CONTROL_AND_HANDLING, headerBuf);
  printf ("MUSE_FILE_CONTROL_AND_HANDLING: %s\n", header -> MUSE_FILE_CONTROL_AND_HANDLING);
  /* MUSE_FILE_RELEASE_INSTRUCTIONS */
  copyField (& offset, & fieldSize, 40, header -> MUSE_FILE_RELEASE_INSTRUCTIONS, headerBuf);
  printf ("MUSE_FILE_RELEASE_INSTRUCTIONS: %s\n", header -> MUSE_FILE_RELEASE_INSTRUCTIONS);
  /* MUSE_FILE_CLASSIFICATION_AUTHORITY */
  copyField (& offset, & fieldSize, 20, header -> MUSE_FILE_CLASSIFICATION_AUTHORITY, headerBuf);
  printf ("MUSE_FILE_CLASSIFICATION_AUTHORITY: %s\n", header -> MUSE_FILE_CLASSIFICATION_AUTHORITY);
  /* MUSE_FILE_SECURITY_CONTROL_NUMBER */
  copyField (& offset, & fieldSize, 20, header -> MUSE_FILE_SECURITY_CONTROL_NUMBER, headerBuf);
  printf ("MUSE_FILE_SECURITY_CONTROL_NUMBER: %s\n", header -> MUSE_FILE_SECURITY_CONTROL_NUMBER);
  /* MUSE_FILE_SECURITY_DOWNGRADE */
  copyField (& offset, & fieldSize, 6, header -> MUSE_FILE_SECURITY_DOWNGRADE, headerBuf);
  printf ("MUSE_FILE_SECURITY_DOWNGRADE: %s\n", header -> MUSE_FILE_SECURITY_DOWNGRADE);
  if (! strcmp ("999998", header -> MUSE_FILE_SECURITY_DOWNGRADE)) {
    /* MUSE_FILE_SECURITY_DOWNGRADING_EVENT */
    copyField (& offset, & fieldSize, 40, header -> MUSE_FILE_SECURITY_DOWNGRADING_EVENT, headerBuf);
    printf ("MUSE_FILE_SECURITY_DOWNGRADING_EVENT: %s\n", header -> MUSE_FILE_SECURITY_DOWNGRADING_EVENT);
  }
#else
  /* File Classification Security System */
  copyField (& offset, & fieldSize, 2, header -> FSCLSY, headerBuf);
  /* File Codewords */                      
  copyField (& offset, & fieldSize, 11, header -> FSCODE, headerBuf);
  /* File Control & Handling */             
  copyField (& offset, & fieldSize, 2, header -> FSCTLH, headerBuf);
  /* File Releasing Instructions */         
  copyField (& offset, & fieldSize, 20, header -> FSREL, headerBuf);
  /* File Declassification Type */          
  copyField (& offset, & fieldSize, 2, header -> FSDCTP, headerBuf);
  /* File Declassification Date */          
  copyField (& offset, & fieldSize, 8, header -> FSDCDT, headerBuf);
  /* File Declassification Exemption */     
  copyField (& offset, & fieldSize, 4, header -> FSDCXM, headerBuf);
  /* File Downgrade */                      
  copyField (& offset, & fieldSize, 1, header -> FSDG, headerBuf);
  /* File Downgrade Date */                 
  copyField (& offset, & fieldSize, 8, header -> FSDGDT, headerBuf);
  /* File Classification Text */            
  copyField (& offset, & fieldSize, 43, header -> FSCLTX, headerBuf);
  /* File Classification Authority Type */  
  copyField (& offset, & fieldSize, 1, header -> FSCLTP, headerBuf);
  /* File Classification Authority */       
  copyField (& offset, & fieldSize, 40, header -> FSCAUT, headerBuf);
  /* File Classification Reason */          
  copyField (& offset, & fieldSize, 1, header -> FSCRSN, headerBuf);
  /* File Security Source Date */           
  copyField (& offset, & fieldSize, 8, header -> FSSRDT, headerBuf);
  /* File Security Control Number */        
  copyField (& offset, & fieldSize, 15, header -> FSCTLN, headerBuf);
#endif
  /* File Copy Number */                    
  copyField (& offset, & fieldSize, 5, header -> FSCOP, headerBuf);
  printf ("File Copy Number: %s\n", header -> FSCOP);
  /* File Number Of Copies */               
  copyField (& offset, & fieldSize, 5, header -> FSCPYS, headerBuf);
  printf ("File Number Of Copies: %s\n", header -> FSCPYS);
  /* Encryption */                          
  copyField (& offset, & fieldSize, 1, header -> ENCRYP, headerBuf);
  printf ("Encryption: %s\n", header -> ENCRYP);
#ifdef USE_MUSE_CODE_AS_SPEC
  /* MUSE_ORIGINATORS_NAME */
  copyField (& offset, & fieldSize, 27, header -> MUSE_ORIGINATORS_NAME, headerBuf);
  printf ("MUSE_ORIGINATORS_NAME: %s\n", header -> MUSE_ORIGINATORS_NAME);
  /* MUSE_ORIGINATORS_PHONE_NUMBER */
  copyField (& offset, & fieldSize, 18, header -> MUSE_ORIGINATORS_PHONE_NUMBER, headerBuf);
  printf ("MUSE_ORIGINATORS_PHONE_NUMBER: %s\n", header -> MUSE_ORIGINATORS_PHONE_NUMBER);
#else
  /* File Background Color */               
  copyField (& offset, & fieldSize, 3, header -> FBKGC, headerBuf);
  /* Originator's Name */                   
  copyField (& offset, & fieldSize, 24, header -> ONAME, headerBuf);
  /* Originator's Phone Number */           
  copyField (& offset, & fieldSize, 18, header -> OPHONE, headerBuf);
#endif
  /* File Length */                         
  copyField (& offset, & fieldSize, 12, header -> FL, headerBuf);
  printf ("File Length: %s\n", header -> FL);
  /* File Header Length (already parsed this) */
  offset += fieldSize;
  fieldSize = 6;

  /* Number of Images */                    
  copyField (& offset, & fieldSize, 3, headerFieldBuf, headerBuf);
  sscanf (headerFieldBuf, "%03d", & header -> NUMI);
  if (sscanf (headerFieldBuf, "%03d", & header -> NUMI) != 1) return SSCANF_FAILED;

  printf ("NUMI: %d\n", header -> NUMI);

  /* get image sizes, if any */
  if (header -> NUMI) {
    int imageIndex;

    if (! (header -> images = (NITFImageSubheader *) malloc (sizeof (NITFImageSubheader) * header -> NUMI))) return MALLOC_FAILED;

    for (imageIndex = 0; imageIndex < header -> NUMI; imageIndex ++) {
      offset += fieldSize;
      fieldSize = 6;
      strncpy (headerFieldBuf, headerBuf + offset, fieldSize);
      headerFieldBuf [fieldSize] = 0;
      if (sscanf (headerFieldBuf, "%06d", & header -> images [imageIndex] . LISHnnn) != 1) return SSCANF_FAILED;
      printf ("Image(%d) LISH: %d\n", imageIndex, header -> images [imageIndex] . LISHnnn);

      offset += fieldSize;
      fieldSize = 10;
      strncpy (headerFieldBuf, headerBuf + offset, fieldSize);
      headerFieldBuf [fieldSize] = 0;
      if (sscanf (headerFieldBuf, "%010d", & header -> images [imageIndex] . LInnn) != 1) return SSCANF_FAILED;
      printf ("Image(%d) LI: %d\n", imageIndex, header -> images [imageIndex] . LInnn);
    }
  } else
    header -> images = 0;

  /* Number of Graphics */
  copyField (& offset, & fieldSize, 3, headerFieldBuf, headerBuf);
  if (sscanf (headerFieldBuf, "%03d", & header -> NUMS) != 1) return SSCANF_FAILED;

  printf ("NUMS: %d\n", header -> NUMS);

  /* get graphic sizes, if any */
  if (header -> NUMS) {
    int graphicIndex;

    if (! (header -> graphics = (NITFGraphicSubheader *) malloc (sizeof (NITFGraphicSubheader) * header -> NUMS))) return MALLOC_FAILED;

    for (graphicIndex = 0; graphicIndex < header -> NUMS; graphicIndex ++) {
      offset += fieldSize;
      fieldSize = 4;
      strncpy (headerFieldBuf, headerBuf + offset, fieldSize);
      headerFieldBuf [fieldSize] = 0;
      if (sscanf (headerFieldBuf, "%04d", & header -> graphics [graphicIndex] . LSSHnnn) != 1) return SSCANF_FAILED;
      printf ("Symbol(%d) LSSH: %d\n", graphicIndex, header -> graphics [graphicIndex] . LSSHnnn);

      offset += fieldSize;
      fieldSize = 6;
      strncpy (headerFieldBuf, headerBuf + offset, fieldSize);
      headerFieldBuf [fieldSize] = 0;
      if (sscanf (headerFieldBuf, "%06d", & header -> graphics [graphicIndex] . LSnnn) != 1) return SSCANF_FAILED;
      printf ("Symbol(%d) LS: %d\n", graphicIndex, header -> graphics [graphicIndex] . LSnnn);
    }
  } else
    header -> graphics = 0;

#ifdef USE_MUSE_CODE_AS_SPEC
  /* Number of Labels */
  copyField (& offset, & fieldSize, 3, headerFieldBuf, headerBuf);
  if (sscanf (headerFieldBuf, "%03d", & header -> MUSE_NUM_LABELS) != 1) return SSCANF_FAILED;
  printf ("MUSE_NUM_LABELS: %d\n", header -> MUSE_NUM_LABELS);

  /* get label sizes, if any */
  if (header -> MUSE_NUM_LABELS) {
    int textIndex;

    for (textIndex = 0; textIndex < header -> MUSE_NUM_LABELS; textIndex ++) {
      offset += fieldSize;
      fieldSize = 4;		/* length of label subheader */

      offset += fieldSize;
      fieldSize = 3;		/* length of label */
    }
  }

  header -> text = 0;
#else
  /* skip NUMX RESERVED FIELD */
  offset += fieldSize;
  fieldSize = 3;
#endif

  /* Number of Text Files */
  copyField (& offset, & fieldSize, 3, headerFieldBuf, headerBuf);
  if (sscanf (headerFieldBuf, "%03d", & header -> NUMT) != 1) return SSCANF_FAILED;
  printf ("NUMT: %d\n", header -> NUMT);

  /* get text file sizes, if any */
  if (header -> NUMT) {
    int textIndex;

    if (! (header -> text = (NITFTextSubheader *) malloc (sizeof (NITFTextSubheader) * header -> NUMT))) return MALLOC_FAILED;

    for (textIndex = 0; textIndex < header -> NUMT; textIndex ++) {
      offset += fieldSize;
      fieldSize = 4;
      strncpy (headerFieldBuf, headerBuf + offset, fieldSize);
      headerFieldBuf [fieldSize] = 0;
      if (sscanf (headerFieldBuf, "%04d", & header -> text [textIndex] . LTSHnnn) != 1) return SSCANF_FAILED;
      printf ("Text(%d) LTSH: %d\n", textIndex, header -> text [textIndex] . LTSHnnn);

      offset += fieldSize;
      fieldSize = 5;
      strncpy (headerFieldBuf, headerBuf + offset, fieldSize);
      headerFieldBuf [fieldSize] = 0;
      if (sscanf (headerFieldBuf, "%05d", & header -> text [textIndex] . LTnnn) != 1) return SSCANF_FAILED;
      printf ("Text(%d) LT: %d\n", textIndex, header -> text [textIndex] . LTnnn);
    }
  } else
    header -> text = 0;

  /* Number of Data Extensions */
  copyField (& offset, & fieldSize, 3, headerFieldBuf, headerBuf);
  if (sscanf (headerFieldBuf, "%03d", & header -> NUMDES) != 1) return SSCANF_FAILED;
  printf ("NUMDES: %d\n", header -> NUMDES);

  /* get data extension sizes, if any */
  if (header -> NUMDES) {
    int dataIndex;

    if (! (header -> data = (NITFDataSubheader *) malloc (sizeof (NITFDataSubheader) * header -> NUMDES))) return MALLOC_FAILED;

    for (dataIndex = 0; dataIndex < header -> NUMDES; dataIndex ++) {
      offset += fieldSize;
      fieldSize = 4;
      strncpy (headerFieldBuf, headerBuf + offset, fieldSize);
      headerFieldBuf [fieldSize] = 0;
      if (sscanf (headerFieldBuf, "%04d", & header -> data [dataIndex] . LDSHnnn) != 1) return SSCANF_FAILED;
      printf ("Data(%d) LDSH: %d\n", dataIndex, header -> data [dataIndex] . LDSHnnn);

      offset += fieldSize;
      fieldSize = 9;
      strncpy (headerFieldBuf, headerBuf + offset, fieldSize);
      headerFieldBuf [fieldSize] = 0;
      if (sscanf (headerFieldBuf, "%09d", & header -> data [dataIndex] . LDnnn) != 1) return SSCANF_FAILED;
      printf ("Data(%d) LD: %d\n", dataIndex, header -> data [dataIndex] . LDnnn);
    }
  } else
    header -> data = 0;

  /* Number of Reserved Extensions */
  copyField (& offset, & fieldSize, 3, headerFieldBuf, headerBuf);
  if (sscanf (headerFieldBuf, "%03d", & header -> NUMRES) != 1) return SSCANF_FAILED;
  printf ("NUMRES: %d\n", header -> NUMRES);

  /* get reserved extension sizes, if any */
  if (header -> NUMRES) {
    int reservedExtensionIndex;

    if (! (header -> reservedExtension = (NITFReservedExtensionSubheader *) malloc (sizeof (NITFReservedExtensionSubheader) * header -> NUMRES))) return MALLOC_FAILED;

    for (reservedExtensionIndex = 0; reservedExtensionIndex < header -> NUMRES; reservedExtensionIndex ++) {
      offset += fieldSize;
      fieldSize = 4;
      strncpy (headerFieldBuf, headerBuf + offset, fieldSize);
      headerFieldBuf [fieldSize] = 0;
      if (sscanf (headerFieldBuf, "%04d", & header -> reservedExtension [reservedExtensionIndex] . LRESHnnn) != 1) return SSCANF_FAILED;
      printf ("ResExt(%d) LRESH: %d\n", reservedExtensionIndex, header -> reservedExtension [reservedExtensionIndex] . LRESHnnn);

      offset += fieldSize;
      fieldSize = 7;
      strncpy (headerFieldBuf, headerBuf + offset, fieldSize);
      headerFieldBuf [fieldSize] = 0;
      if (sscanf (headerFieldBuf, "%07d", & header -> reservedExtension [reservedExtensionIndex] . LREnnn) != 1) return SSCANF_FAILED;
      printf ("ResExt(%d) LRE: %d\n", reservedExtensionIndex, header -> reservedExtension [reservedExtensionIndex] . LREnnn);
    }
  } else
    header -> reservedExtension = 0;

  /* Number of User Defined Header Data */
  copyField (& offset, & fieldSize, 5, headerFieldBuf, headerBuf);
  if (sscanf (headerFieldBuf, "%05d", & header -> UDHDL) != 1) return SSCANF_FAILED;
  printf ("UDHDL %d\n", header -> UDHDL);
  
  /* get user defined header sizes, if any */
  if (header -> UDHDL > 0) {
    if (! (header -> userDefined = (NITFUserDefinedSubheader *) malloc (sizeof (NITFUserDefinedSubheader)))) return MALLOC_FAILED;

    /* User Defined Header Overflow */
    copyField (& offset, & fieldSize, 3, header -> userDefined -> UDHOFL, headerBuf);
    printf ("UDHOFL: %s\n", header -> userDefined -> UDHOFL);
  
    /* User Defined Header Data
       This may have embedded nulls, so the struct doesn't include an
       extra byte for a terminating null, and we use memcpy instead of
       copyField here. */
    offset += fieldSize;
    fieldSize = header -> UDHDL - 3;
    header -> userDefined -> UDHD = (char *) malloc (fieldSize);
    memcpy (header -> userDefined -> UDHD, headerBuf + offset, fieldSize);
  } else
    header -> userDefined = 0;

  /* Number of Extended Headers */
  copyField (& offset, & fieldSize, 5, headerFieldBuf, headerBuf);
  if (sscanf (headerFieldBuf, "%05d", & header -> XHDL) != 1) return SSCANF_FAILED;
  printf ("XHDL: %d\n", header -> XHDL);
  
  /* get extended header sizes, if any */
  if (header -> XHDL > 0) {
    if (! (header -> extended = (NITFExtendedSubheader *) malloc (sizeof (NITFExtendedSubheader)))) return MALLOC_FAILED;

    /* Extended Header Overflow */
    copyField (& offset, & fieldSize, 3, header -> extended -> XHDOFL, headerBuf);
    printf ("XDHOFL: %s\n", header -> extended -> XHDOFL);

    /* Extended Header Data
       See comment above on User Defined Header Data. */
    offset += fieldSize;
    fieldSize = header -> XHDL - 3;
    header -> extended -> XHD = (char *) malloc (fieldSize);
    memcpy (header -> extended -> XHD, headerBuf + offset, fieldSize);
  } else
    header -> extended = 0;

  free (headerBuf);
  free (headerFieldBuf);

  /* Get the image headers */
  {
    int imageIndex;
    char * imgHdrBuf, * imgFieldBuf;

    for (imageIndex = 0; imageIndex < header -> NUMI; imageIndex ++) {
      /* malloc and read image header */
      if (! (imgHdrBuf = (char *) malloc (header -> images [imageIndex] . LISHnnn))) return MALLOC_FAILED;
      if (! (imgFieldBuf = (char *) malloc (header -> images [imageIndex] . LISHnnn))) return MALLOC_FAILED;
      if (fread (imgHdrBuf, header -> images [imageIndex] . LISHnnn, 1, nitf) != 1) return FREAD_FAILED;

      /* skip image */
      if (fseek (nitf, header -> images [imageIndex] . LInnn, SEEK_CUR)) return FSEEK_FAILED;

      /* parse image header */
      offset = fieldSize = 0;

      /* File Part Type */
      copyField (& offset, & fieldSize, 2, header -> images [imageIndex] . IM, imgHdrBuf);
      printf ("Image(%d) IM: %s\n", imageIndex, header -> images [imageIndex] . IM);
      /* Image ID Part 1 */
      copyField (& offset, & fieldSize, 10, header -> images [imageIndex] . IID1, imgHdrBuf);
      printf ("Image(%d) IID1: %s\n", imageIndex, header -> images [imageIndex] . IID1);
      /* Image Date & Time */
      copyField (& offset, & fieldSize, 14, header -> images [imageIndex] . IDATIM, imgHdrBuf);
      printf ("Image(%d) IDATIM: %s\n", imageIndex, header -> images [imageIndex] . IDATIM);
      /* Target ID */
      copyField (& offset, & fieldSize, 17, header -> images [imageIndex] . TGTID, imgHdrBuf);
      printf ("Image(%d) TGTID: %s\n", imageIndex, header -> images [imageIndex] . TGTID);
      /* Image ID Part 2 */
      copyField (& offset, & fieldSize, 80, header -> images [imageIndex] . IID2, imgHdrBuf);
      printf ("Image(%d) IID2: %s\n", imageIndex, header -> images [imageIndex] . IID2);
      /* Image Security Classification */
      copyField (& offset, & fieldSize, 1, header -> images [imageIndex] . ISCLAS, imgHdrBuf);
      printf ("Image(%d) ISCLAS: %s\n", imageIndex, header -> images [imageIndex] . ISCLAS);
      /* Image Security Classification System */
      copyField (& offset, & fieldSize, 2, header -> images [imageIndex] . ISCLSY, imgHdrBuf);
      printf ("Image(%d) ISCLSY: %s\n", imageIndex, header -> images [imageIndex] . ISCLSY);
      /* Image Codewords */
      copyField (& offset, & fieldSize, 11, header -> images [imageIndex] . ISCODE, imgHdrBuf);
      printf ("Image(%d) ISCODE: %s\n", imageIndex, header -> images [imageIndex] . ISCODE);
      /* Image Control & Handling */
      copyField (& offset, & fieldSize, 2, header -> images [imageIndex] . ISCTLH, imgHdrBuf);
      printf ("Image(%d) ISCTLH: %s\n", imageIndex, header -> images [imageIndex] . ISCTLH);
      /* Image Releasing Instructions */
      copyField (& offset, & fieldSize, 20, header -> images [imageIndex] . ISREL, imgHdrBuf);
      printf ("Image(%d) ISREL: %s\n", imageIndex, header -> images [imageIndex] . ISREL);
      /* Image Declassification Type */
      copyField (& offset, & fieldSize, 2, header -> images [imageIndex] . ISDCTP, imgHdrBuf);
      printf ("Image(%d) ISDCTP: %s\n", imageIndex, header -> images [imageIndex] . ISDCTP);
      /* Image Declassification Date */
      copyField (& offset, & fieldSize, 8, header -> images [imageIndex] . ISDCDT, imgHdrBuf);
      printf ("Image(%d) ISDCDT: %s\n", imageIndex, header -> images [imageIndex] . ISDCDT);
      /* Image Declassification Exemption */
      copyField (& offset, & fieldSize, 4, header -> images [imageIndex] . ISDCXM, imgHdrBuf);
      printf ("Image(%d) ISDCXM: %s\n", imageIndex, header -> images [imageIndex] . ISDCXM);
      /* Image Downgrade */
      copyField (& offset, & fieldSize, 1, header -> images [imageIndex] . ISDG, imgHdrBuf);
      printf ("Image(%d) ISDG: %s\n", imageIndex, header -> images [imageIndex] . ISDG);
      /* Image Downgrade Date */
      copyField (& offset, & fieldSize, 8, header -> images [imageIndex] . ISDGDT, imgHdrBuf);
      printf ("Image(%d) ISDGDT: %s\n", imageIndex, header -> images [imageIndex] . ISDGDT);
      /* Image Classification Text */
      copyField (& offset, & fieldSize, 43, header -> images [imageIndex] . ISCLTX, imgHdrBuf);
      printf ("Image(%d) ISCLTX: %s\n", imageIndex, header -> images [imageIndex] . ISCLTX);
      /* Image Classification Authority Type */
      copyField (& offset, & fieldSize, 1, header -> images [imageIndex] . ISCATP, imgHdrBuf);
      printf ("Image(%d) ISCATP: %s\n", imageIndex, header -> images [imageIndex] . ISCATP);
      /* Image Classification Authority */
      copyField (& offset, & fieldSize, 40, header -> images [imageIndex] . ISCAUT, imgHdrBuf);
      printf ("Image(%d) ISCAUT: %s\n", imageIndex, header -> images [imageIndex] . ISCAUT);
      /* Image Classification Reason */
      copyField (& offset, & fieldSize, 1, header -> images [imageIndex] . ISCRSN, imgHdrBuf);
      printf ("Image(%d) ISCRSN: %s\n", imageIndex, header -> images [imageIndex] . ISCRSN);
      /* Image Classification Source Date */
      copyField (& offset, & fieldSize, 8, header -> images [imageIndex] . ISSRDT, imgHdrBuf);
      printf ("Image(%d) ISSRDT: %s\n", imageIndex, header -> images [imageIndex] . ISSRDT);
      /* Image Security Control Number */
      copyField (& offset, & fieldSize, 15, header -> images [imageIndex] . ISCTLN, imgHdrBuf);
      printf ("Image(%d) ISCTLN: %s\n", imageIndex, header -> images [imageIndex] . ISCTLN);
      /* Encryption */
      copyField (& offset, & fieldSize, 1, header -> images [imageIndex] . ENCRYP, imgHdrBuf);
      printf ("Image(%d) ENCRYP: %s\n", imageIndex, header -> images [imageIndex] . ENCRYP);
      /* Image Source */
      copyField (& offset, & fieldSize, 42, header -> images [imageIndex] . ISORCE, imgHdrBuf);
      printf ("Image(%d) ISORCE: %s\n", imageIndex, header -> images [imageIndex] . ISORCE);

      /* Number of Significant Rows in Image */
      copyField (& offset, & fieldSize, 8, imgFieldBuf, imgHdrBuf);
      printf ("3 scanning 08d from \"%s\"\n", imgFieldBuf);
      if (sscanf (imgFieldBuf, "%08d", & header -> images [imageIndex] . NROWS) != 1) return SSCANF_FAILED;
      printf ("4\n");

      /* Number of Significant Columns in Image */
      copyField (& offset, & fieldSize, 8, imgFieldBuf, imgHdrBuf);
      if (sscanf (imgFieldBuf, "%08d", & header -> images [imageIndex] . NCOLS) != 1) return SSCANF_FAILED;

      /* Pixel Value Type */
      copyField (& offset, & fieldSize, 3, header -> images [imageIndex] . PVTYPE, imgHdrBuf);
      /* Image Representation */
      copyField (& offset, & fieldSize, 8, header -> images [imageIndex] . IREP, imgHdrBuf);
      /* Image Category */
      copyField (& offset, & fieldSize, 8, header -> images [imageIndex] . ICAT, imgHdrBuf);
      /* Actual Bits-Per-Pixel Per Band */
      copyField (& offset, & fieldSize, 2, header -> images [imageIndex] . ABPP, imgHdrBuf);
      /* Pixel Justification */
      copyField (& offset, & fieldSize, 1, header -> images [imageIndex] . PJUST, imgHdrBuf);
      /* Image Coordinate System */
      copyField (& offset, & fieldSize, 1, header -> images [imageIndex] . ICORDS, imgHdrBuf);

      /* Image Geographic Location */
      copyField (& offset, & fieldSize, 60, imgFieldBuf, imgHdrBuf);
      if (scanACorner (imgFieldBuf, 0,
		       & header -> images [imageIndex] . ULLat,
		       & header -> images [imageIndex] . ULLon) ||
	  scanACorner (imgFieldBuf, 1,
		       & header -> images [imageIndex] . URLat,
		       & header -> images [imageIndex] . URLon) ||
	  scanACorner (imgFieldBuf, 2,
		       & header -> images [imageIndex] . LRLat,
		       & header -> images [imageIndex] . LRLon) ||
	  scanACorner (imgFieldBuf, 3,
		       & header -> images [imageIndex] . LLLat,
		       & header -> images [imageIndex] . LLLon)) {

	header -> images [imageIndex] . ULLat = 0.0;
	header -> images [imageIndex] . ULLon = 0.0;
	header -> images [imageIndex] . URLat = 0.0;
	header -> images [imageIndex] . URLon = 0.0;
	header -> images [imageIndex] . LRLat = 0.0;
	header -> images [imageIndex] . LRLon = 0.0;
	header -> images [imageIndex] . LLLat = 0.0;
	header -> images [imageIndex] . LLLon = 0.0;
      }

      /* Number of Image Comments */
      copyField (& offset, & fieldSize, 1, imgFieldBuf, imgHdrBuf);
      if (sscanf (imgFieldBuf, "%1d", & header -> images [imageIndex] . NICOM) != 1) return SSCANF_FAILED;

      /* get image comments, if any */
      if (header -> images [imageIndex] . NICOM) {
	int commentIndex;

	/* malloc array of pointers for comments */
	if (! (header -> images [imageIndex] . ICOMn =
	       (char **) malloc (sizeof (char *) * header -> images [imageIndex] . NICOM))) return MALLOC_FAILED;

	for (commentIndex = 0; commentIndex < header -> images [imageIndex] . NICOM; commentIndex ++) {
	  /* malloc comment string */
	  if (! (header -> images [imageIndex] . ICOMn [commentIndex] = malloc (81))) return MALLOC_FAILED;

	  /* copy comment */
	  copyField (& offset, & fieldSize, 80, header -> images [imageIndex] . ICOMn [commentIndex], imgHdrBuf);
	  header -> images [imageIndex] . ICOMn [commentIndex] [80] = '\0';
	}
      } else
	header -> images [imageIndex] . ICOMn = 0;

      /* Image Compression */
      copyField (& offset, & fieldSize, 2, header -> images [imageIndex] . IC, imgHdrBuf);

      /* Compression Rate Code */
      if (strcmp (header -> images [imageIndex] . IC, "NC") &&
	  strcmp (header -> images [imageIndex] . IC, "NM"))
	copyField (& offset, & fieldSize, 4, header -> images [imageIndex] . COMRAT, imgHdrBuf);
      else
	header -> images [imageIndex] . COMRAT [0] = '\0';

      {
	int band, bands;

	/* Number of Bands */
	copyField (& offset, & fieldSize, 1, imgFieldBuf, imgHdrBuf);
	if (sscanf (imgFieldBuf, "%1d", & header -> images [imageIndex] . NBANDS) != 1) return SSCANF_FAILED;

	if (header -> images [imageIndex] . NBANDS) {
	  bands = header -> images [imageIndex] . NBANDS;
	  header -> images [imageIndex] . XBANDS = 0;
	} else {
	  /* Number of Multi-Spectral Bands */
	  copyField (& offset, & fieldSize, 5, imgFieldBuf, imgHdrBuf);
	  if (sscanf (imgFieldBuf, "%05d", & header -> images [imageIndex] . XBANDS) != 1) return SSCANF_FAILED;
	  bands = header -> images [imageIndex] . XBANDS;
	}

	if (bands) {
	  if (! (header -> images [imageIndex] . bands = (NITFBand *) malloc (sizeof (NITFBand) * bands))) return MALLOC_FAILED;

	  for (band = 0; band < bands; band ++) {
	    /* nnth Band Representation */
	    copyField (& offset, & fieldSize, 2, header -> images [imageIndex] . bands [band] . IREPBANDnn, imgHdrBuf);
	    /* nnth Band Subcategory */
	    copyField (& offset, & fieldSize, 6, header -> images [imageIndex] . bands [band] . ISUBCATnn, imgHdrBuf);
	    /* nnth Band Image Filter Condition */
	    copyField (& offset, & fieldSize, 1, header -> images [imageIndex] . bands [band] . IFCnn, imgHdrBuf);
	    /* nnth Band Standard Image Filter Code */
	    copyField (& offset, & fieldSize, 3, header -> images [imageIndex] . bands [band] . IMFLTnn, imgHdrBuf);

	    /* nnth Band Number of LUTs */
	    copyField (& offset, & fieldSize, 1, imgFieldBuf, imgHdrBuf);
	    if (sscanf (imgFieldBuf, "%1d", & header -> images [imageIndex] . bands [band] . NLUTSnn) != 1) return SSCANF_FAILED;

	    /* get LUTS, if any */
	    if (header -> images [imageIndex] . bands [band] . NLUTSnn) {
	      /* nnth Band Number of LUT Entries */
	      copyField (& offset, & fieldSize, 5, imgFieldBuf, imgHdrBuf);
	      if (sscanf (imgFieldBuf, "%05d", & header -> images [imageIndex] . bands [band] . NELUTnn) != 1) return SSCANF_FAILED;

	      /* malloc LUT pointer array */
	      if (! (header -> images [imageIndex] . bands [band] . LUTDnnm =
		     (char **) malloc (sizeof (char *) * header -> images [imageIndex] . bands [band] . NLUTSnn))) return MALLOC_FAILED;

	      {
		int lut;

		for (lut = 0; lut < header -> images [imageIndex] . bands [band] . NLUTSnn; lut ++) {
		  /* malloc and copy a LUT */
		  offset += fieldSize;
		  fieldSize = header -> images [imageIndex] . bands [band] . NELUTnn;
		  if (! (header -> images [imageIndex] . bands [band] . LUTDnnm [lut] =
			 malloc (header -> images [imageIndex] . bands [band] . NELUTnn))) return MALLOC_FAILED;
		  memcpy (header -> images [imageIndex] . bands [band] . LUTDnnm [lut], imgHdrBuf + offset, fieldSize);
		}
	      }
	    } else {
	      header -> images [imageIndex] . bands [band] . NELUTnn = 0;
	      header -> images [imageIndex] . bands [band] . LUTDnnm = 0;
	    }
	  }
	} else
	  header -> images [imageIndex] . bands = 0;
      }

      /* Image Sync Mode */
      copyField (& offset, & fieldSize, 1, header -> images [imageIndex] . ISYNC, imgHdrBuf);
      /* Image Mode */
      copyField (& offset, & fieldSize, 1, header -> images [imageIndex] . IMODE, imgHdrBuf);

      /* Number of Blocks Per Row */
      copyField (& offset, & fieldSize, 4, imgFieldBuf, imgHdrBuf);
      if (sscanf (imgFieldBuf, "%04d", & header -> images [imageIndex] . NBPR) != 1) return SSCANF_FAILED;

      /* Number of Blocks Per Column */
      copyField (& offset, & fieldSize, 4, imgFieldBuf, imgHdrBuf);
      if (sscanf (imgFieldBuf, "%04d", & header -> images [imageIndex] . NBPC) != 1) return SSCANF_FAILED;

      /* Number of Pixels Per Block Horizontal */
      copyField (& offset, & fieldSize, 4, imgFieldBuf, imgHdrBuf);
      if (sscanf (imgFieldBuf, "%04d", & header -> images [imageIndex] . NPPBH) != 1) return SSCANF_FAILED;

      /* Number of Pixels Per Block Vertical */
      copyField (& offset, & fieldSize, 4, imgFieldBuf, imgHdrBuf);
      if (sscanf (imgFieldBuf, "%04d", & header -> images [imageIndex] . NPPBV) != 1) return SSCANF_FAILED;

      /* Number of Bits Per Pixel Per Band */
      copyField (& offset, & fieldSize, 2, imgFieldBuf, imgHdrBuf);
      if (sscanf (imgFieldBuf, "%02d", & header -> images [imageIndex] . NBPP) != 1) return SSCANF_FAILED;

      /* Display Level */
      copyField (& offset, & fieldSize, 3, header -> images [imageIndex] . IDLVL, imgHdrBuf);
      /* Attachment Level */
      copyField (& offset, & fieldSize, 3, header -> images [imageIndex] . IALVL, imgHdrBuf);
      /* Image Location */
      copyField (& offset, & fieldSize, 10, header -> images [imageIndex] . ILOC, imgHdrBuf);
      /* Image Magnification */
      copyField (& offset, & fieldSize, 4, header -> images [imageIndex] . IMAG, imgHdrBuf);

      /* User Defined Image Data Length */
      copyField (& offset, & fieldSize, 5, imgFieldBuf, imgHdrBuf);
      if (sscanf (imgFieldBuf, "%05d", & header -> images [imageIndex] . UDIDL) != 1) return SSCANF_FAILED;

      /* copy user defined image data, if any */
      if (header -> images [imageIndex] . UDIDL) {
	/* User Defined Overflow */
	copyField (& offset, & fieldSize, 3, header -> images [imageIndex] . UDOFL, imgHdrBuf);

	/* malloc and copy user defined image data */
	offset += fieldSize;
	fieldSize = header -> images [imageIndex] . UDIDL - 3;
	header -> images [imageIndex] . UDID = malloc (fieldSize);
	memcpy (header -> images [imageIndex] . UDID, headerBuf + offset, fieldSize);
      } else {
	header -> images [imageIndex] . UDOFL [0] = '\0';
	header -> images [imageIndex] . UDID = 0;
      }
	    
      /* Extended Subheader Image Data Length */
      copyField (& offset, & fieldSize, 5, imgFieldBuf, imgHdrBuf);
      if (sscanf (imgFieldBuf, "%05d", & header -> images [imageIndex] . IXSHDL) != 1) return SSCANF_FAILED;

      /* copy extended subheader data, if any */
      if (header -> images [imageIndex] . IXSHDL) {
	/* Extended Subheader Overflow */
	copyField (& offset, & fieldSize, 3, header -> images [imageIndex] . IXSOFL, imgHdrBuf);

	/* malloc and copy extended subheader data */
	offset += fieldSize;
	fieldSize = header -> images [imageIndex] . IXSHDL - 3;
	header -> images [imageIndex] . IXSHD = malloc (fieldSize);
	memcpy (header -> images [imageIndex] . IXSHD, headerBuf + offset, fieldSize);
      } else {
	header -> images [imageIndex] . IXSOFL [0] = '\0';
	header -> images [imageIndex] . IXSHD = 0;
      }

      free (imgHdrBuf);
    }
  }


  return NITFTOOLS_SUCCESS;
}

int NITF_extractImageToFile (FILE * nitf, NITFFileHeader * header, int imageIndex, char * outPath) {
  int i, sum = 0;

  /* seek to end of file header */
  if (fseek (nitf, header -> HL, SEEK_SET)) return FSEEK_FAILED;

  /* skip length of images and image headers before the image of interest */
  for (i = 0; i < imageIndex; i ++)
    sum += header -> images [i] . LISHnnn + /* header length */
      header -> images [i] . LInnn; /* image length */
	
  sum += header -> images [imageIndex] . LISHnnn; /* header length */

  if (fseek (nitf, sum, SEEK_CUR)) return FSEEK_FAILED;

  {
    char * rowOfTiles;
    int tileRow;
    int bytesPerPixel = header -> images [imageIndex] . NBPP == 8 ? 1 : 2;
    int blocksPerRow = header -> images [imageIndex] . NBPR;
    int blocksPerColumn = header -> images [imageIndex] . NBPC;
    int pixelsPerBlockH = header -> images [imageIndex] . NPPBH;
    int pixelsPerBlockV = header -> images [imageIndex] . NPPBV;
    int tileRowSizeInBytes =
      bytesPerPixel *
      blocksPerRow *
      pixelsPerBlockH *
      pixelsPerBlockV;
    int line, tile;
    FILE * outImage;
     
    char * imageLine;		/* test */
    
    if (! (imageLine = malloc (blocksPerRow * pixelsPerBlockH * bytesPerPixel))) return MALLOC_FAILED; /* test */

    if (! (outImage = fopen (outPath, "w"))) return FOPEN_FAILED;

    /* malloc a buffer for one row of tiles */
    if (! (rowOfTiles = (char *) malloc (tileRowSizeInBytes))) {
      fclose (outImage);
      return MALLOC_FAILED;
    }

    /* for each row of tiles */
    for (tileRow = 0; tileRow < blocksPerColumn; tileRow ++) {
      /* read a row of tiles */
      if (fread (rowOfTiles, tileRowSizeInBytes, 1, nitf) != 1) {
	fclose (outImage);
	free (rowOfTiles);
	return FREAD_FAILED;
      }

      /* write the tile row line by line */
      for (line = 0; line < pixelsPerBlockV; line ++) {
	for (tile = 0; tile < blocksPerRow; tile ++) {
	  if (fwrite (rowOfTiles +
		      tile * pixelsPerBlockH * pixelsPerBlockV * bytesPerPixel +
		      line * pixelsPerBlockH * bytesPerPixel,
		      pixelsPerBlockH * bytesPerPixel, 1, outImage) != 1) {
	    fclose (outImage);
	    free (rowOfTiles);
	    return FWRITE_FAILED;
	  }
	}
      }
    }

    fclose (outImage);
    free (rowOfTiles);
  }

  return NITFTOOLS_SUCCESS;
}

int NITF_paddedImageLineSizeInBytes (NITFFileHeader * header,int imageIndex) {
  return header -> images [imageIndex] . NBPR *
    header -> images [imageIndex] . NPPBH *
    (header -> images [imageIndex] . NBPP == 8 ? 1 : 2);
}

int NITF_extractImageLine (FILE * nitf, NITFFileHeader * header, int imageIndex, int line, char * lineBuf) {
  static char * rowOfTiles = 0;
  static int rowOfTilesSize = 0;
  static int lastReadPos = -1;
  static FILE * lastNitf = 0;

  int i, sum = 0;
  int tileRow;
  int bytesPerPixel = header -> images [imageIndex] . NBPP == 8 ? 1 : 2;
  int blocksPerRow = header -> images [imageIndex] . NBPR;
  int pixelsPerBlockH = header -> images [imageIndex] . NPPBH;
  int pixelsPerBlockV = header -> images [imageIndex] . NPPBV;
  int tileRowSizeInBytes =
    bytesPerPixel *
    blocksPerRow *
    pixelsPerBlockH *
    pixelsPerBlockV;
  int tile;
  int resized = 0;

  /* malloc a buffer for one row of tiles */
  if (rowOfTilesSize != tileRowSizeInBytes) {
    if (! (rowOfTiles = (char *) realloc (rowOfTiles, tileRowSizeInBytes))) return REALLOC_FAILED;
    rowOfTilesSize = tileRowSizeInBytes;
    resized = 1;
  }

  /* skip to end of file header */
  sum = header -> HL;

  /* skip length of images and image headers before the image of interest */
  for (i = 0; i < imageIndex; i ++)
    sum += header -> images [i] . LISHnnn + /* header length */
      header -> images [i] . LInnn; /* image length */
	
  sum += header -> images [imageIndex] . LISHnnn; /* header length */

    /* seek to the row and mod line to the row */
  tileRow = line / pixelsPerBlockV;
  line = line % pixelsPerBlockV;

  sum += tileRow * tileRowSizeInBytes;

  if (resized ||
      nitf != lastNitf ||
      sum != lastReadPos) {
    lastNitf = nitf;
    lastReadPos = sum;

    if (fseek (nitf, sum, SEEK_SET)) return FSEEK_FAILED;

    /* read a row of tiles */
    if (fread (rowOfTiles, tileRowSizeInBytes, 1, nitf) != 1) return FREAD_FAILED;
  }

  for (tile = 0; tile < blocksPerRow; tile ++)
    memcpy (lineBuf + tile * pixelsPerBlockH * bytesPerPixel,
	      rowOfTiles +
	      tile * pixelsPerBlockH * pixelsPerBlockV * bytesPerPixel +
	      line * pixelsPerBlockH * bytesPerPixel,
	      pixelsPerBlockH * bytesPerPixel);

  return NITFTOOLS_SUCCESS;
}

#ifdef DEMO
int extractImageToFile_usingExtractImageLine (FILE * nitf, NITFFileHeader * header, int imageIndex, char * outPath) {
  FILE * outImage;
  int line, nl = header -> images [imageIndex] . NBPC * header -> images [imageIndex] . NPPBV;
  int lineSize = NITF_paddedImageLineSizeInBytes (header, imageIndex);
  char * lineBuf = malloc (lineSize);
     
  if (! lineBuf) return MALLOC_FAILED;
  if (! (outImage = fopen (outPath, "w"))) return FOPEN_FAILED;

  for (line = 0; line < nl; line ++) {
    if (NITF_extractImageLine (nitf, header, imageIndex, line, lineBuf)) {
      fclose (outImage);
      return EXTRACTIMAGELINE_FAILED;
    }    

    if (fwrite (lineBuf, lineSize, 1, outImage) != 1) {
      fclose (outImage);
      return FWRITE_FAILED;
    }
  }

  fclose (outImage);
  free (lineBuf);

  return NITFTOOLS_SUCCESS;
}
#endif
