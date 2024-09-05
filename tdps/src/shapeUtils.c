#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include <string.h>
#include <tcl.h>

#include "shapefil.h"
#include "polyClip.h"
#include "linesIntersect.h"
#include "utm.h"
#include "tranmerc.h"
#include "mgrsfns.h"

static int xyInAOI (double x, double y, double aoiN, double aoiS, double aoiE, double aoiW) {
  if (aoiE > aoiW) {
    if (x < aoiW || x > aoiE) return 0;
  } else {			/* straddles 180 */
    if (x < aoiW && x > aoiE) return 0;
  }

  if (y < aoiS || y > aoiN) return 0;

  return 1;
}

/*
  usage: getShapeFile shapeFile.shp scalePower aoiN aoiS aoiE aoiW

  Requires companion shapeFile.dbf.

  Reads shp and dbf files, returning a list:

  {shapeFile.shp shapeType shapeCount dbfFields {{vecId {x y x y ... x y} {ps1 ps2 ... psn}} {vecId {x y x y ... x y} {ps1 ... psn}} ...}}

  {x y x y ... x y} is the vector vertex coordinates, scaled by 2**scalePower

  psn is the zero-based offset of the start vertex of the nth part of the vector

  Note that shapeCount is the count in the file, not the number of
  vectors returned. dbfFields is a list of field descriptions of the form
  {fieldName fieldType width decimals}
 */
static int getShapeFileProc (ClientData clientData, Tcl_Interp *interp, int argc, CONST char * argv []) {
  char * usage = "wrong # args: should be \"getShapeFile shapefile scalePower aoiN aoiS aoiE aoiW\"";
  SHPHandle shapeHandle;
  DBFHandle dbfHandle;
  int nEntities, nShapetype;
  int dbfFieldCount, dbfRecordCount;
  double xyzmMin [4], xyzmMax [4];
  Tcl_Obj * shapeType = 0;
  int scalePower;
  double aoiN, aoiS, aoiE, aoiW;

  if (argc != 7) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (sscanf (argv [2], "%d", & scalePower) != 1 || scalePower < 0 || scalePower > 20) {
    Tcl_SetResult (interp, "scalePower must be an integer in the range 0 .. 20", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (sscanf (argv [3], "%30lf", & aoiN) != 1 || aoiN > 90.0 || aoiN < -90.0) {
    Tcl_SetResult (interp, "aoiN must be a float in the range -90 .. 90", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (sscanf (argv [4], "%30lf", & aoiS) != 1 || aoiS > 90.0 || aoiS < -90.0) {
    Tcl_SetResult (interp, "aoiS must be a float in the range -90 .. 90", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (sscanf (argv [5], "%30lf", & aoiE) != 1 || aoiE > 180.0 || aoiE < -180.0) {
    Tcl_SetResult (interp, "aoiE must be a float in the range -180 .. 180", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (sscanf (argv [6], "%30lf", & aoiW) != 1 || aoiW > 180.0 || aoiW < -180.0) {
    Tcl_SetResult (interp, "aoiW must be a float in the range -180 .. 180", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (! (shapeHandle = SHPOpen (argv [1], "rb")) ||
      ! (dbfHandle = DBFOpen (argv [1], "rb"))) {
    if (shapeHandle)
      SHPClose (shapeHandle);
    {
      char buf [1000];
      sprintf (buf, "error loading %s.shp or %s.dbf", argv [1], argv [1]);
      Tcl_SetResult (interp, buf, TCL_VOLATILE);
    }
    return TCL_ERROR;
  }

  SHPGetInfo (shapeHandle, & nEntities, & nShapetype, xyzmMin, xyzmMax);

  {
    char * msgBuf = malloc (strlen (argv [1]) + 100);
    int supportedType = 0;

    /* check shape type */
    switch (nShapetype) {
    case SHPT_NULL:
      sprintf (msgBuf, "%s contains an unsupported shape entity type (NULL)\n", argv [1]); break;
    case SHPT_POINT:
      sprintf (msgBuf, "%s contains an unsupported shape entity type (POINT)\n", argv [1]); break;
    case SHPT_ARC:
      shapeType = Tcl_NewStringObj ("ARC", -1);
      supportedType = 1;
      break;
    case SHPT_POLYGON:
      shapeType = Tcl_NewStringObj ("POLYGON", -1);
      supportedType = 1;
      break;
    case SHPT_MULTIPOINT:
      sprintf (msgBuf, "%s contains an unsupported shape entity type (MULTIPOINT)\n", argv [1]); break;
    case SHPT_POINTZ:
      sprintf (msgBuf, "%s contains an unsupported shape entity type (POINTZ)\n", argv [1]); break;
    case SHPT_ARCZ:
      sprintf (msgBuf, "%s contains an unsupported shape entity type (ARCZ)\n", argv [1]); break;
    case SHPT_POLYGONZ:
      sprintf (msgBuf, "%s contains an unsupported shape entity type (POLYGONZ)\n", argv [1]); break;
    case SHPT_MULTIPOINTZ:
      sprintf (msgBuf, "%s contains an unsupported shape entity type (MULTIPOINTZ)\n", argv [1]); break;
    case SHPT_POINTM:
      sprintf (msgBuf, "%s contains an unsupported shape entity type (POINTM)\n", argv [1]); break;
    case SHPT_ARCM:
      sprintf (msgBuf, "%s contains an unsupported shape entity type (ARCM)\n", argv [1]); break;
    case SHPT_POLYGONM:
      sprintf (msgBuf, "%s contains an unsupported shape entity type (POLYGONM)\n", argv [1]); break;
    case SHPT_MULTIPOINTM:
      sprintf (msgBuf, "%s contains an unsupported shape entity type (MULTIPOINTM)\n", argv [1]); break;
    case SHPT_MULTIPATCH:
      sprintf (msgBuf, "%s contains an unsupported shape entity type (MULTIPATCH)\n", argv [1]); break;
    default:
      sprintf (msgBuf, "%s contains an unsupported shape entity type (UNKNOWN)\n", argv [1]); break;
    }

    if (! supportedType) {
      Tcl_SetResult (interp, msgBuf, TCL_VOLATILE);
      free (msgBuf);
      SHPClose (shapeHandle);
      DBFClose (dbfHandle);
      return TCL_ERROR;
    }

    free (msgBuf);
  }

  dbfFieldCount = DBFGetFieldCount (dbfHandle);
  dbfRecordCount = DBFGetRecordCount (dbfHandle);

  if (nEntities != dbfRecordCount) {
    fprintf (stderr, "getShapeFileProc: nEntities %d dbfRecordCount %d\n", nEntities, dbfRecordCount);
    Tcl_SetResult (interp, "shp and dbf lengths do not match", TCL_VOLATILE);
    SHPClose (shapeHandle);
    DBFClose (dbfHandle);
    return TCL_ERROR;
  }

  {
    SHPObject * shapeObject;
    int entity, vertex, fieldCount, field;
    char fieldName [12];
    int fieldType, fieldWidth, fieldDecimals;
    Tcl_Obj * returnValue [5], * returnValueList;
    Tcl_Obj ** vectors, ** coords = 0, * vector [4], ** startIndices = 0;
/*      Tcl_Obj ** rawCoords = 0; */
    Tcl_Obj ** fields;
    Tcl_Obj * fieldDescription [4];
    int * inAOI;
    int aoiEntity, aoiEntities = 0;
    int part;

    inAOI = (int *) malloc (sizeof (int) * nEntities);
    for (entity = 0; entity < nEntities; entity ++) {
      shapeObject = SHPReadObject (shapeHandle, entity);

      inAOI [entity] = 0;
      for (vertex = 0; vertex < shapeObject -> nVertices; vertex ++)
	if (xyInAOI (shapeObject -> padfX [vertex], shapeObject -> padfY [vertex], aoiN, aoiS, aoiE, aoiW)) {
	  inAOI [entity] = 1;
	  aoiEntities ++;
	  break;
	}
      
      SHPDestroyObject (shapeObject);
    }
    
    vectors = (Tcl_Obj **) malloc (sizeof (Tcl_Obj *) * aoiEntities);
    for (entity = aoiEntity = 0; entity < nEntities; entity ++) {
      if (inAOI [entity]) {
	shapeObject = SHPReadObject (shapeHandle, entity);

	coords = (Tcl_Obj **) malloc (sizeof (Tcl_Obj *) * shapeObject -> nVertices * 2);
/*  	rawCoords = (Tcl_Obj **) malloc (sizeof (Tcl_Obj *) * shapeObject -> nVertices * 2); */

	for (vertex = 0; vertex < shapeObject -> nVertices; vertex ++) {
/*  	  rawCoords [2 * vertex + 0] = Tcl_NewDoubleObj (shapeObject -> padfX [vertex]); */
	  coords [2 * vertex + 0] = Tcl_NewDoubleObj (shapeObject -> padfX [vertex] * pow (2.0, scalePower));
	  /* invert vertical sign for display */
/*  	  rawCoords [2 * vertex + 1] = Tcl_NewDoubleObj (shapeObject -> padfY [vertex]); */
	  coords [2 * vertex + 1] = Tcl_NewDoubleObj (- shapeObject -> padfY [vertex] * pow (2.0, scalePower));
	}

	vector [0] = Tcl_NewIntObj (entity);
	vector [1] = Tcl_NewListObj (shapeObject -> nVertices * 2, coords);
/*  	vector [3] = Tcl_NewListObj (shapeObject -> nVertices * 2, rawCoords); */
	free (coords);
/*  	free (rawCoords); */

	/* part start indices */
	if (shapeObject -> nParts > 1) {
	  startIndices = (Tcl_Obj **) malloc (sizeof (Tcl_Obj *) * shapeObject -> nParts);
	  for (part = 0; part < shapeObject -> nParts; part ++)
	    startIndices [part] = Tcl_NewIntObj (shapeObject -> panPartStart [part]);
	  vector [2] = Tcl_NewListObj (shapeObject -> nParts, startIndices);
	  free (startIndices);
	} else
	  vector [2] = Tcl_NewIntObj (0);

	vectors [aoiEntity] = Tcl_NewListObj (3, vector);

	SHPDestroyObject (shapeObject);

	aoiEntity ++;
      }
    }

    fieldCount = DBFGetFieldCount (dbfHandle);
    fields = (Tcl_Obj **) malloc (sizeof (Tcl_Obj *) * fieldCount);

    /* A field description looks like this {fieldName fieldType width decimals} */
    for (field = 0; field < fieldCount; field ++) {
      fieldType = DBFGetFieldInfo (dbfHandle, field, fieldName, & fieldWidth, & fieldDecimals);

      fieldDescription [0] = Tcl_NewStringObj (fieldName, -1);
      switch (fieldType) {
      case FTString:
	fieldDescription [1] = Tcl_NewStringObj ("FTString", -1);
	break;
      case FTInteger:
	fieldDescription [1] = Tcl_NewStringObj ("FTInteger", -1);
	break;
      case FTDouble:
	fieldDescription [1] = Tcl_NewStringObj ("FTDouble", -1);
	break;
      default:
	fieldDescription [1] = Tcl_NewStringObj ("FTInvalid", -1);
	break;
      }
      fieldDescription [2] = Tcl_NewIntObj (fieldWidth);
      fieldDescription [3] = Tcl_NewIntObj (fieldDecimals);

      fields [field] = Tcl_NewListObj (4, fieldDescription);
    }
    
    returnValue [0] = Tcl_NewStringObj (argv [1], -1);
    returnValue [1] = shapeType;
    returnValue [2] = Tcl_NewIntObj (aoiEntities);
    returnValue [3] = Tcl_NewListObj (fieldCount, fields);
    returnValue [4] = Tcl_NewListObj (aoiEntities, vectors);

    returnValueList = Tcl_NewListObj (5, returnValue);

    free (inAOI);
    free (fields);
    free (vectors);

    Tcl_SetObjResult (interp, returnValueList);
  }

  SHPClose (shapeHandle);
  DBFClose (dbfHandle);

  return TCL_OK;
}

static int getVectorPropertiesProc (ClientData clientData, Tcl_Interp *interp, int argc, CONST char * argv []) {
  char * usage = "wrong # args: should be \"getVectorProperties shapefile vecId\"";
  DBFHandle dbfHandle;
  int dbfFieldCount, dbfRecordCount;
  int vecId;
  Tcl_Obj ** returnValue, * returnValueList;
  int realFieldCount, realField;
  int field;
  DBFFieldType fieldType;
  char fieldName [13];
  int fieldWidth;
  int decimals;

  if (argc != 3) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (sscanf (argv [2], "%d", & vecId) != 1 || vecId < 0) {
    Tcl_SetResult (interp, "error parsing vecId", TCL_VOLATILE);
    return TCL_ERROR;
  }
    
  if (! (dbfHandle = DBFOpen (argv [1], "rb"))) {
    Tcl_SetResult (interp, "error loading shapefile.dbf", TCL_VOLATILE);
    return TCL_ERROR;
  }

  dbfFieldCount = DBFGetFieldCount (dbfHandle);
  dbfRecordCount = DBFGetRecordCount (dbfHandle);

  if (vecId >= dbfRecordCount) {
    char buf [1000];
    sprintf (buf, "vecId %d not contained in %s, which has only %d records\n", vecId, argv [1], dbfRecordCount);
    Tcl_SetResult (interp, buf, TCL_VOLATILE);
    DBFClose (dbfHandle);
    return TCL_ERROR;
  }

  /* count the number of fields with supported types (string, integer, double) */
  for (field = realFieldCount = 0; field < dbfFieldCount; field ++) {
    fieldType = DBFGetFieldInfo (dbfHandle, field, fieldName, & fieldWidth, & decimals);
    switch (fieldType) {
    case FTString:
    case FTInteger:
    case FTDouble:
      realFieldCount ++;
    default:
      ; /* ignore uknown field types */
    }
  }

  returnValue = (Tcl_Obj **) malloc (sizeof (Tcl_Obj *) * 2 * realFieldCount); /* name and value for each field */

  for (field = realField = 0; field < dbfFieldCount; field ++, realField += 2) {
    fieldType = DBFGetFieldInfo (dbfHandle, field, fieldName, & fieldWidth, & decimals);
    switch (fieldType) {
    case FTString:
      returnValue [realField] = Tcl_NewStringObj (fieldName, -1);
      returnValue [realField + 1] = Tcl_NewStringObj (DBFReadStringAttribute (dbfHandle, vecId, field), -1);
      break;
    case FTInteger:
      returnValue [realField] = Tcl_NewStringObj (fieldName, -1);
      returnValue [realField + 1] = Tcl_NewIntObj (DBFReadIntegerAttribute (dbfHandle, vecId, field));
      break;
    case FTDouble:
      returnValue [realField] = Tcl_NewStringObj (fieldName, -1);
      returnValue [realField + 1] = Tcl_NewDoubleObj (DBFReadDoubleAttribute (dbfHandle, vecId, field));
      break;
    default:
      realField -= 2;
    }
  }

  DBFClose (dbfHandle);

  returnValueList = Tcl_NewListObj (2 * realFieldCount, returnValue);
  free (returnValue);
  Tcl_SetObjResult (interp, returnValueList);

  return TCL_OK;
}

/* this should be factored out with getVectorPropertiesProc */
static int getVectorPropertiesFromOpenFileProc (ClientData clientData, Tcl_Interp *interp, int argc, CONST char * argv []) {
  char * usage = "wrong # args: should be \"getVectorPropertiesFromOpenFile dbfHandle vecId\"";
  DBFHandle dbfHandle;
  int dbfFieldCount, dbfRecordCount;
  int vecId;
  Tcl_Obj ** returnValue, * returnValueList;
  int realFieldCount, realField;
  int field;
  DBFFieldType fieldType;
  char fieldName [13];
  int fieldWidth;
  int decimals;
  long longDbfHandle;

  if (argc != 3) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (sscanf (argv [1], "%ld", & longDbfHandle) != 1 || longDbfHandle < 0) {
    char msgBuf [100];
    sprintf (msgBuf, "error parsing dbfHandle from \"%s\"", argv [1]);
    Tcl_SetResult (interp, msgBuf, TCL_VOLATILE);
    return TCL_ERROR;
  }
  dbfHandle = (DBFHandle) longDbfHandle;
    
  if (sscanf (argv [2], "%d", & vecId) != 1 || vecId < 0) {
    Tcl_SetResult (interp, "error parsing vecId", TCL_VOLATILE);
    return TCL_ERROR;
  }
    
  dbfFieldCount = DBFGetFieldCount (dbfHandle);
  dbfRecordCount = DBFGetRecordCount (dbfHandle);

  if (vecId >= dbfRecordCount) {
    char buf [1000];
    sprintf (buf, "vecId %d not contained in %s, which has only %d records\n", vecId, argv [1], dbfRecordCount);
    Tcl_SetResult (interp, buf, TCL_VOLATILE);
    DBFClose (dbfHandle);
    return TCL_ERROR;
  }

  /* count the number of fields with supported types (string, integer, double) */
  for (field = realFieldCount = 0; field < dbfFieldCount; field ++) {
    fieldType = DBFGetFieldInfo (dbfHandle, field, fieldName, & fieldWidth, & decimals);
    switch (fieldType) {
    case FTString:
    case FTInteger:
    case FTDouble:
      realFieldCount ++;
    default:
      ; /* ignore uknown field types */
    }
  }

  returnValue = (Tcl_Obj **) malloc (sizeof (Tcl_Obj *) * 2 * realFieldCount); /* name and value for each field */

  for (field = realField = 0; field < dbfFieldCount; field ++, realField += 2) {
    fieldType = DBFGetFieldInfo (dbfHandle, field, fieldName, & fieldWidth, & decimals);
    switch (fieldType) {
    case FTString:
      returnValue [realField] = Tcl_NewStringObj (fieldName, -1);
      returnValue [realField + 1] = Tcl_NewStringObj (DBFReadStringAttribute (dbfHandle, vecId, field), -1);
      break;
    case FTInteger:
      returnValue [realField] = Tcl_NewStringObj (fieldName, -1);
      returnValue [realField + 1] = Tcl_NewIntObj (DBFReadIntegerAttribute (dbfHandle, vecId, field));
      break;
    case FTDouble:
      returnValue [realField] = Tcl_NewStringObj (fieldName, -1);
      returnValue [realField + 1] = Tcl_NewDoubleObj (DBFReadDoubleAttribute (dbfHandle, vecId, field));
      break;
    default:
      realField -= 2;
    }
  }

  returnValueList = Tcl_NewListObj (2 * realFieldCount, returnValue);
  free (returnValue);
  Tcl_SetObjResult (interp, returnValueList);

  return TCL_OK;
}

static int DBFCreateProc (ClientData clientData, Tcl_Interp *interp, int argc, CONST char * argv []) {
  char * usage = "wrong # args: should be \"DBFCreate dbfFile\"";
  long result;
  char resultStr [20];

  if (argc != 2) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  result = (long) DBFCreate (argv [1]);

  if (result) {
    sprintf (resultStr, "%ld", result);
    Tcl_SetResult (interp, resultStr, TCL_VOLATILE);
  } else {
    Tcl_SetResult (interp, "DBFCreate failed", TCL_VOLATILE);
    return TCL_ERROR;
  }

  return TCL_OK;
}

static int SHPCreateProc (ClientData clientData, Tcl_Interp *interp, int argc, CONST char * argv []) {
  char * usage = "wrong # args: should be \"SHPCreate shapefile type\"";
  long result;
  char resultStr [20];

  if (argc != 3) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (! strcmp (argv [2], "NULL"))
    result = (long) SHPCreate (argv [1], SHPT_NULL);
  else if (! strcmp (argv [2], "POINT"))
    result = (long) SHPCreate (argv [1], SHPT_POINT);
  else if (! strcmp (argv [2], "ARC"))
    result = (long) SHPCreate (argv [1], SHPT_ARC);
  else if (! strcmp (argv [2], "POLYGON"))
    result = (long) SHPCreate (argv [1], SHPT_POLYGON);
  else if (! strcmp (argv [2], "MULTIPOINT"))
    result = (long) SHPCreate (argv [1], SHPT_MULTIPOINT);
  else if (! strcmp (argv [2], "POINTZ"))
    result = (long) SHPCreate (argv [1], SHPT_POINTZ);
  else if (! strcmp (argv [2], "ARCZ"))
    result = (long) SHPCreate (argv [1], SHPT_ARCZ);
  else if (! strcmp (argv [2], "POLYGONZ"))
    result = (long) SHPCreate (argv [1], SHPT_POLYGONZ);
  else if (! strcmp (argv [2], "MULTIPOINTZ"))
    result = (long) SHPCreate (argv [1], SHPT_MULTIPOINTZ);
  else if (! strcmp (argv [2], "POINTM"))
    result = (long) SHPCreate (argv [1], SHPT_POINTM);
  else if (! strcmp (argv [2], "ARCM"))
    result = (long) SHPCreate (argv [1], SHPT_ARCM);
  else if (! strcmp (argv [2], "POLYGONM"))
    result = (long) SHPCreate (argv [1], SHPT_POLYGONM);
  else if (! strcmp (argv [2], "MULTIPOINTM"))
    result = (long) SHPCreate (argv [1], SHPT_MULTIPATCH);
  else if (! strcmp (argv [2], "MULTIPOINTM"))
    result = (long) SHPCreate (argv [1], SHPT_MULTIPATCH);
  else
    result = (long) SHPCreate (argv [1], SHPT_MULTIPATCH);

  if (result) {
    sprintf (resultStr, "%ld", result);
    Tcl_SetResult (interp, resultStr, TCL_VOLATILE);
  } else {
    Tcl_SetResult (interp, "SHPCreate failed", TCL_VOLATILE);
    return TCL_ERROR;
  }

  return TCL_OK;
}

static int DBFOpenProc (ClientData clientData, Tcl_Interp *interp, int argc, CONST char * argv []) {
  char * usage = "wrong # args: should be \"DBFOpen shapefile\"";
  long result;

  if (argc != 2) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  result = (long) DBFOpen (argv [1], "rb");

  if (result) {
    Tcl_SetObjResult (interp, Tcl_NewIntObj (result));
  } else {
    Tcl_SetResult (interp, "DBFOpen failed", TCL_VOLATILE);
    return TCL_ERROR;
  }

  return TCL_OK;
}

static int SHPOpenProc (ClientData clientData, Tcl_Interp *interp, int argc, CONST char * argv []) {
  char * usage = "wrong # args: should be \"SHPOpen shapefile\"";
  long result;
  char resultStr [20];

  if (argc != 2) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  result = (long) SHPOpen (argv [1], "rb");

  if (result) {
    sprintf (resultStr, "%ld", result);
    Tcl_SetResult (interp, resultStr, TCL_VOLATILE);
  } else {
    Tcl_SetResult (interp, "SHPOpen failed", TCL_VOLATILE);
    return TCL_ERROR;
  }

  return TCL_OK;
}

static int DBFAddFieldProc (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv []) {
  char * usage = "wrong # args: should be \"DBFAddField dbfHandle name type width decimals\"";
  long longDbfHandle;
  DBFHandle dbfHandle;
  char * fieldName;
  char * fieldType;
  int fieldWidth;
  int fieldDecimals;
  int result;
  char resultStr [20];

  if (objc != 6) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetLongFromObj (interp, objv [1], & longDbfHandle) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing dbfHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  dbfHandle = (DBFHandle) longDbfHandle;

  if (Tcl_GetIntFromObj (interp, objv [4], & fieldWidth) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing width", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetIntFromObj (interp, objv [5], & fieldDecimals) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing decimals", TCL_VOLATILE);
    return TCL_ERROR;
  }

  fieldName = strdup (Tcl_GetStringFromObj (objv [2], 0));
  fieldType = strdup (Tcl_GetStringFromObj (objv [3], 0));

  if (! strcmp (fieldType, "FTString"))
    result = DBFAddField (dbfHandle, fieldName, FTString, fieldWidth, fieldDecimals);
  else if (! strcmp (fieldType, "FTInteger"))
    result = DBFAddField (dbfHandle, fieldName, FTInteger, fieldWidth, fieldDecimals);
  else if (! strcmp (fieldType, "FTDouble"))
    result = DBFAddField (dbfHandle, fieldName, FTDouble, fieldWidth, fieldDecimals);
  else
    result = DBFAddField (dbfHandle, fieldName, FTInvalid, fieldWidth, fieldDecimals);

  if (result < 0) {
    Tcl_SetResult (interp, "DBFAddField failed", TCL_VOLATILE);
    free (fieldName);
    free (fieldType);
    return TCL_ERROR;
  }

  sprintf (resultStr, "%d", result);

  Tcl_SetResult (interp, resultStr, TCL_VOLATILE);

  free (fieldName);
  free (fieldType);
  return TCL_OK;
}

static int DBFWriteIntegerProc (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv []) {
  char * usage = "wrong # args: should be \"DBFWriteInteger dbfHandle shapeIndex fieldIndex value\"";
  long longDbfHandle;
  DBFHandle dbfHandle;
  int shapeIndex, fieldIndex, value;

  if (objc != 5) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetLongFromObj (interp, objv [1], & longDbfHandle) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing dbfHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  dbfHandle = (DBFHandle) longDbfHandle;

  if (Tcl_GetIntFromObj (interp, objv [2], & shapeIndex) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing shapeIndex", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetIntFromObj (interp, objv [3], & fieldIndex) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing fieldIndex", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetIntFromObj (interp, objv [4], & value) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing value", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (! DBFWriteIntegerAttribute (dbfHandle, shapeIndex, fieldIndex, value)) {
    Tcl_SetResult (interp, "DBFWriteIntegerAttribute failed", TCL_VOLATILE);
    return TCL_ERROR;
  }

  Tcl_SetResult (interp, "", TCL_VOLATILE);

  return TCL_OK;
}

static int DBFCopyIntegerProc (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv []) {
  char * usage = "wrong # args: should be \"DBFCopyInteger fromDbfHandle fromShapeIndex fromFieldIndex toDbfHandle toShapeIndex toFieldIndex\"";
  long longDbfHandle;
  DBFHandle fromDbfHandle, toDbfHandle;
  int fromShapeIndex, fromFieldIndex;
  int toShapeIndex, toFieldIndex;

  if (objc != 7) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetLongFromObj (interp, objv [1], & longDbfHandle) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing fromDbfHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  fromDbfHandle = (DBFHandle) longDbfHandle;

  if (Tcl_GetLongFromObj (interp, objv [4], & longDbfHandle) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing toDbfHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  toDbfHandle = (DBFHandle) longDbfHandle;

  if (Tcl_GetIntFromObj (interp, objv [2], & fromShapeIndex) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing fromShapeIndex", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetIntFromObj (interp, objv [5], & toShapeIndex) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing toShapeIndex", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetIntFromObj (interp, objv [3], & fromFieldIndex) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing fromFieldIndex", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetIntFromObj (interp, objv [6], & toFieldIndex) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing toFieldIndex", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (! DBFWriteIntegerAttribute
      (toDbfHandle, toShapeIndex, toFieldIndex,
       DBFReadIntegerAttribute (fromDbfHandle, fromShapeIndex, fromFieldIndex))) {
    Tcl_SetResult (interp, "DBFWriteIntegerAttribute failed", TCL_VOLATILE);
    return TCL_ERROR;
  }

  Tcl_SetResult (interp, "", TCL_VOLATILE);

  return TCL_OK;
}

static int DBFWriteDoubleProc (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv []) {
  char * usage = "wrong # args: should be \"DBFWriteDouble dbfHandle shapeIndex fieldIndex value\"";
  long longDbfHandle;
  DBFHandle dbfHandle;
  int shapeIndex, fieldIndex;
  double value;

  if (objc != 5) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetLongFromObj (interp, objv [1], & longDbfHandle) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing dbfHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  dbfHandle = (DBFHandle) longDbfHandle;

  if (Tcl_GetIntFromObj (interp, objv [2], & shapeIndex) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing shapeIndex", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetIntFromObj (interp, objv [3], & fieldIndex) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing fieldIndex", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetDoubleFromObj (interp, objv [4], & value) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing value", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (! DBFWriteDoubleAttribute (dbfHandle, shapeIndex, fieldIndex, value)) {
    Tcl_SetResult (interp, "DBFWriteDoubleAttribute failed", TCL_VOLATILE);
    return TCL_ERROR;
  }

  Tcl_SetResult (interp, "", TCL_VOLATILE);

  return TCL_OK;
}

static int DBFCopyDoubleProc (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv []) {
  char * usage = "wrong # args: should be \"DBFCopyDouble fromDbfHandle fromShapeIndex fromFieldIndex toDbfHandle toShapeIndex toFieldIndex\"";
  long longDbfHandle;
  DBFHandle fromDbfHandle, toDbfHandle;
  int fromShapeIndex, fromFieldIndex;
  int toShapeIndex, toFieldIndex;

  if (objc != 7) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetLongFromObj (interp, objv [1], & longDbfHandle) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing fromDbfHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  fromDbfHandle = (DBFHandle) longDbfHandle;

  if (Tcl_GetLongFromObj (interp, objv [4], & longDbfHandle) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing toDbfHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  toDbfHandle = (DBFHandle) longDbfHandle;

  if (Tcl_GetIntFromObj (interp, objv [2], & fromShapeIndex) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing fromShapeIndex", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetIntFromObj (interp, objv [5], & toShapeIndex) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing toShapeIndex", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetIntFromObj (interp, objv [3], & fromFieldIndex) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing fromFieldIndex", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetIntFromObj (interp, objv [6], & toFieldIndex) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing toFieldIndex", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (! DBFWriteDoubleAttribute
      (toDbfHandle, toShapeIndex, toFieldIndex,
       DBFReadDoubleAttribute (fromDbfHandle, fromShapeIndex, fromFieldIndex))) {
    Tcl_SetResult (interp, "DBFWriteDoubleAttribute failed", TCL_VOLATILE);
    return TCL_ERROR;
  }

  Tcl_SetResult (interp, "", TCL_VOLATILE);

  return TCL_OK;
}

static int DBFCopySchemaProc (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv []) {
  char * usage = "wrong # args: should be \"DBFCopySchema fromDbfHandle toDbfHandle\"";
  long longDbfHandle;
  DBFHandle fromDbfHandle, toDbfHandle;
  int fieldCount, field;
  DBFFieldType fieldType;
  char fieldName [13];
  int fieldWidth;
  int decimals;
  int result = 0;

  if (objc != 3) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetLongFromObj (interp, objv [1], & longDbfHandle) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing fromDbfHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  fromDbfHandle = (DBFHandle) longDbfHandle;

  if (Tcl_GetLongFromObj (interp, objv [2], & longDbfHandle) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing toDbfHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  toDbfHandle = (DBFHandle) longDbfHandle;

  fieldCount = DBFGetFieldCount (fromDbfHandle);

  for (field = 0; field < fieldCount; field ++) {
    fieldType = DBFGetFieldInfo (fromDbfHandle, field, fieldName, & fieldWidth, & decimals);
    switch (fieldType) {
    case FTString:
      result = DBFAddField (toDbfHandle, fieldName, FTString, fieldWidth, decimals); break;
    case FTInteger:
      result = DBFAddField (toDbfHandle, fieldName, FTInteger, fieldWidth, decimals); break;
    case FTDouble:
      result = DBFAddField (toDbfHandle, fieldName, FTDouble, fieldWidth, decimals); break;
    default:
      ;
    }

    if (result < 0) {
      Tcl_SetResult (interp, "DBFAddField failed", TCL_VOLATILE);
      return TCL_ERROR;
    }
  }

  Tcl_SetResult (interp, "", TCL_VOLATILE);

  return TCL_OK;
}

static int DBFWriteStringProc (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv []) {
  char * usage = "wrong # args: should be \"DBFWriteString dbfHandle shapeIndex fieldIndex value\"";
  long longDbfHandle;
  DBFHandle dbfHandle;
  int shapeIndex, fieldIndex;
  char * value;

  if (objc != 5) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetLongFromObj (interp, objv [1], & longDbfHandle) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing dbfHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  dbfHandle = (DBFHandle) longDbfHandle;

  if (Tcl_GetIntFromObj (interp, objv [2], & shapeIndex) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing shapeIndex", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetIntFromObj (interp, objv [3], & fieldIndex) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing fieldIndex", TCL_VOLATILE);
    return TCL_ERROR;
  }

  value = Tcl_GetStringFromObj (objv [4], 0);

  if (! DBFWriteStringAttribute (dbfHandle, shapeIndex, fieldIndex, value)) {
    Tcl_SetResult (interp, "DBFWriteStringAttribute failed", TCL_VOLATILE);
    return TCL_ERROR;
  }

  Tcl_SetResult (interp, "", TCL_VOLATILE);

  return TCL_OK;
}

static int SHPGetInfoProc (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv []) {
  char * usage = "wrong # args: should be \"SHPGetInfo shpHandle\"";
  long longShpHandle;
  SHPHandle shpHandle;
  int nEntities, nShapetype;
  double xyzmMin [4], xyzmMax [4];
  char buf [100], buf2 [200];

  if (objc != 2) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetLongFromObj (interp, objv [1], & longShpHandle) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing shpHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  shpHandle = (SHPHandle) longShpHandle;

  SHPGetInfo (shpHandle, & nEntities, & nShapetype, xyzmMin, xyzmMax);

  sprintf (buf, "nEntities %d nShapeType ", nEntities);

  switch (nShapetype) {
  case SHPT_NULL:
    strcat (buf, "NULL"); break;
  case SHPT_POINT:
    strcat (buf, "POINT"); break;
  case SHPT_ARC:
    strcat (buf, "ARC"); break;
  case SHPT_POLYGON:
    strcat (buf, "POLYGON"); break;
  case SHPT_MULTIPOINT:
    strcat (buf, "MULTIPOINT"); break;
  case SHPT_POINTZ:
    strcat (buf, "POINTZ"); break;
  case SHPT_ARCZ:
    strcat (buf, "ARCZ"); break;
  case SHPT_POLYGONZ:
    strcat (buf, "POLYGONZ"); break;
  case SHPT_MULTIPOINTZ:
    strcat (buf, "MULTIPOINTZ"); break;
  case SHPT_POINTM:
    strcat (buf, "POINTM"); break;
  case SHPT_ARCM:
    strcat (buf, "ARCM"); break;
  case SHPT_POLYGONM:
    strcat (buf, "POLYGONM"); break;
  case SHPT_MULTIPOINTM:
    strcat (buf, "MULTIPOINTM"); break;
  case SHPT_MULTIPATCH:
    strcat (buf, "MULTIPATCH"); break;
  default:
    strcat (buf, "UNKNOWN"); break;
  }

  sprintf (buf2, "%s xMin %.16f yMin %.16f zMin %.16f mMin %.16f xMax %.16f yMax %.16f zMax %.16f mMax %.16f",
	   buf, xyzmMin [0], xyzmMin [1], xyzmMin [2], xyzmMin [3], xyzmMax [0], xyzmMax [1], xyzmMax [2], xyzmMax [3]);

  Tcl_SetObjResult (interp, Tcl_NewStringObj (buf2, -1));

  return TCL_OK;
}

static int SHPGetEntityCountProc (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv []) {
  char * usage = "wrong # args: should be \"SHPGetEntityCount shpHandle\"";
  long longShpHandle;
  SHPHandle shpHandle;
  int nEntities, nShapetype;
  double xyzmMin [4], xyzmMax [4];

  if (objc != 2) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetLongFromObj (interp, objv [1], & longShpHandle) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing shpHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  shpHandle = (SHPHandle) longShpHandle;

  SHPGetInfo (shpHandle, & nEntities, & nShapetype, xyzmMin, xyzmMax);

  Tcl_SetObjResult (interp, Tcl_NewIntObj (nEntities));

  return TCL_OK;
}

static int SHPGetVerticesProc (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv []) {
  char * usage = "wrong # args: should be \"SHPGetVertices shpHandle shapeId\"";
  long longShpHandle;
  SHPHandle shpHandle;
  int shpId;
  int nEntities, nShapetype;
  double xyzmMin [4], xyzmMax [4];

  if (objc != 3) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetLongFromObj (interp, objv [1], & longShpHandle) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing shpHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  shpHandle = (SHPHandle) longShpHandle;

  if (Tcl_GetIntFromObj (interp, objv [2], & shpId) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing shpId", TCL_VOLATILE);
    return TCL_ERROR;
  }

  SHPGetInfo (shpHandle, & nEntities, & nShapetype, xyzmMin, xyzmMax);

  if (shpId >= nEntities) {
    Tcl_SetResult (interp, "shpId >= nEntities", TCL_VOLATILE);
    return TCL_ERROR;
  }

  {
    SHPObject * shapeObject;
    int vertex;
    Tcl_Obj ** coords = 0;
    Tcl_Obj * coordList = 0;

    shapeObject = SHPReadObject (shpHandle, shpId);

    coords = (Tcl_Obj **) malloc (sizeof (Tcl_Obj *) * shapeObject -> nVertices * 2);

    for (vertex = 0; vertex < shapeObject -> nVertices; vertex ++) {
      coords [2 * vertex + 0] = Tcl_NewDoubleObj (shapeObject -> padfX [vertex]);
      /* invert vertical sign for display */
      coords [2 * vertex + 1] = Tcl_NewDoubleObj (- shapeObject -> padfY [vertex]);
    }

    coordList = Tcl_NewListObj (shapeObject -> nVertices * 2, coords);

    free (coords);
    SHPDestroyObject (shapeObject);

    Tcl_SetObjResult (interp, coordList);
  }

  return TCL_OK;
}

static int DBFGetInfoProc (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv []) {
  char * usage = "wrong # args: should be \"DBFGetInfo dbfHandle\"";
  long longDbfHandle;
  DBFHandle dbfHandle;
  int recordCount, fieldCount;
  int field;
  Tcl_Obj ** fields;
  char fieldName [12];
  int fieldType, fieldWidth, fieldDecimals;
  Tcl_Obj * returnValue, * fieldsObj;
  Tcl_Obj * fieldDescription [4], * returnList [6];

  if (objc != 2) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetLongFromObj (interp, objv [1], & longDbfHandle) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing dbfHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  dbfHandle = (DBFHandle) longDbfHandle;

  recordCount = DBFGetRecordCount (dbfHandle);
  fieldCount = DBFGetFieldCount (dbfHandle);

  fields = (Tcl_Obj **) malloc (sizeof (Tcl_Obj *) * fieldCount);

  /* A field description looks like this {fieldName fieldType width decimals} */
  for (field = 0; field < fieldCount; field ++) {
    fieldType = DBFGetFieldInfo (dbfHandle, field, fieldName, & fieldWidth, & fieldDecimals);

    fieldDescription [0] = Tcl_NewStringObj (fieldName, -1);
    switch (fieldType) {
    case FTString:
      fieldDescription [1] = Tcl_NewStringObj ("FTString", -1);
      break;
    case FTInteger:
      fieldDescription [1] = Tcl_NewStringObj ("FTInteger", -1);
      break;
    case FTDouble:
      fieldDescription [1] = Tcl_NewStringObj ("FTDouble", -1);
      break;
    default:
      fieldDescription [1] = Tcl_NewStringObj ("FTInvalid", -1);
      break;
    }
    fieldDescription [2] = Tcl_NewIntObj (fieldWidth);
    fieldDescription [3] = Tcl_NewIntObj (fieldDecimals);

    fields [field] = Tcl_NewListObj (4, fieldDescription);
  }
    
  fieldsObj = Tcl_NewListObj (fieldCount, fields);

  free (fields);

  returnList [0] = Tcl_NewStringObj ("recordCount", -1);
  returnList [1] = Tcl_NewIntObj (recordCount);
  returnList [2] = Tcl_NewStringObj ("fieldCount", -1);
  returnList [3] = Tcl_NewIntObj (fieldCount);
  returnList [4] = Tcl_NewStringObj ("fieldDescriptions", -1);
  returnList [5] = fieldsObj;

  returnValue = Tcl_NewListObj (6, returnList);

  Tcl_SetObjResult (interp, returnValue);

  return TCL_OK;
}

static int DBFGetRecordCountProc (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv []) {
  char * usage = "wrong # args: should be \"DBFGetRecordCount dbfHandle\"";
  long longDbfHandle;
  DBFHandle dbfHandle;

  if (objc != 2) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetLongFromObj (interp, objv [1], & longDbfHandle) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing dbfHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  dbfHandle = (DBFHandle) longDbfHandle;

  Tcl_SetObjResult (interp, Tcl_NewIntObj (DBFGetRecordCount (dbfHandle)));

  return TCL_OK;
}

#define pointInBox(x, y, top, right, left, bottom) ((x) >= (left) && (x) <= (right) && (y) >= (bottom) && (y) <= (top))

static int shapeTouchesBoxProc (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv []) {
  char * usage = "wrong # args: should be \"shapeTouchesBox shpHandle vectorId s n w e\"";
  long longShpHandle;
  int vectorId;
  SHPHandle shpHandle;
  int touchesBox = 0;
  double s, n, w, e;
  SHPObject * shpObject;

  if (objc != 7) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetLongFromObj (interp, objv [1], & longShpHandle) != TCL_OK || longShpHandle < 0) {
    Tcl_SetResult (interp, "error parsing shpHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  shpHandle = (SHPHandle) longShpHandle;

  if (Tcl_GetIntFromObj (interp, objv [2], & vectorId) != TCL_OK || vectorId < 0) {
    Tcl_SetResult (interp, "error parsing vectorId", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetDoubleFromObj (interp, objv [3], & s) != TCL_OK || s < -90.0 || s > 90.0) {
    Tcl_SetResult (interp, "error parsing s", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetDoubleFromObj (interp, objv [4], & n) != TCL_OK || n < -90.0 || n > 90.0) {
    Tcl_SetResult (interp, "error parsing n", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetDoubleFromObj (interp, objv [5], & w) != TCL_OK || w < -180.0 || w > 180.0) {
    Tcl_SetResult (interp, "error parsing w", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetDoubleFromObj (interp, objv [6], & e) != TCL_OK || e < -180.0 || e > 180.0) {
    Tcl_SetResult (interp, "error parsing e", TCL_VOLATILE);
    return TCL_ERROR;
  }

  shpObject = SHPReadObject (shpHandle, vectorId);

  touchesBox =
    pointInBox(shpObject -> dfXMin, shpObject -> dfYMax, n, e, w, s) ||
    pointInBox(shpObject -> dfXMax, shpObject -> dfYMax, n, e, w, s) ||
    pointInBox(shpObject -> dfXMax, shpObject -> dfYMin, n, e, w, s) ||
    pointInBox(shpObject -> dfXMin, shpObject -> dfYMin, n, e, w, s);

  /*   if (! touchesBox) */
  /*     printf ("doesn't touch: obj xMin %lf xMax %lf yMin %lf yMax %lf box n %lf e %lf w %lf s %lf\n", */
  /* 	    shpObject -> dfXMin, shpObject -> dfXMax, shpObject -> dfYMin, shpObject -> dfYMax, n, e, w, s); */

  SHPDestroyObject (shpObject);

  Tcl_SetObjResult (interp, Tcl_NewIntObj (touchesBox));

  return TCL_OK;
}

/* Preconditions:
   dbfHandleIn is valid DBFHandle on file opened for reading
   dbfHandleOut is valid DBFHandle on file opened for writing
   indexIn is valid index in input file of record to copy
   indexOut is valid index for new record in output file
   fieldTypes is dbfFieldCount long, matching input file, each holding one of (FTString, FTDouble, FTInteger, FTInvalid)

   Returns: zero on success, non-zero otherwise */
static int DBFCopyRecord (int dbfFieldCount, int * fieldTypes, DBFHandle dbfHandleIn, DBFHandle dbfHandleOut, int indexIn, int indexOut) {
  int fieldIndex;
  char * stringValue;

  for (fieldIndex = 0; fieldIndex < dbfFieldCount; fieldIndex ++) {
    switch (fieldTypes [fieldIndex]) {
    case FTString:
      stringValue = (char *) DBFReadStringAttribute (dbfHandleIn, indexIn, fieldIndex);
      if (stringValue) {
	stringValue = strdup (stringValue);
      } else {
	stringValue = strdup ("");
      }
      if (! DBFWriteStringAttribute (dbfHandleOut, indexOut, fieldIndex, stringValue)) {
	return 1;
      }
      free (stringValue);
      break;

    case FTDouble:
      if (! DBFWriteDoubleAttribute
	  (dbfHandleOut, indexOut, fieldIndex, DBFReadDoubleAttribute (dbfHandleIn, indexIn, fieldIndex))) {
	return 1;
      }
      break;

    case FTInteger:
      if (! DBFWriteIntegerAttribute
	  (dbfHandleOut, indexOut, fieldIndex, DBFReadIntegerAttribute (dbfHandleIn, indexIn, fieldIndex))) {
	return 1;
      }
      break;

    case FTInvalid:
      if (! DBFWriteIntegerAttribute (dbfHandleOut, indexOut, fieldIndex, 0)) {
	return 1;
      }
    } /* switch (fieldType) */
  } /* for (fieldIndex = 0; fieldIndex < dbfFieldCount; fieldIndex ++) */

  return 0;
}

static int DBFCopyRecordProc (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv []) {
  char * usage = "wrong # args: should be \"DBFCopyRecord fromDbfHandle fromRecordIndex toDbfHandle toRecordIndex\"";
  long longDbfHandle;
  DBFHandle fromDbfHandle, toDbfHandle;
  int fromRecordIndex, toRecordIndex;
  int fieldCount;
  char fieldName [13];
  int fieldWidth;
  int decimals;

  if (objc != 5) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetLongFromObj (interp, objv [1], & longDbfHandle) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing fromDbfHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  fromDbfHandle = (DBFHandle) longDbfHandle;

  if (Tcl_GetIntFromObj (interp, objv [2], & fromRecordIndex) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing fromRecordIndex", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetLongFromObj (interp, objv [3], & longDbfHandle) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing toDbfHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  toDbfHandle = (DBFHandle) longDbfHandle;

  if (Tcl_GetIntFromObj (interp, objv [4], & toRecordIndex) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing toRecordIndex", TCL_VOLATILE);
    return TCL_ERROR;
  }

  fieldCount = DBFGetFieldCount (fromDbfHandle);

  {
    int * fieldTypes = (int *) malloc (sizeof (int) * fieldCount);
    int fieldIndex;

    for (fieldIndex = 0; fieldIndex < fieldCount; fieldIndex ++)
      fieldTypes [fieldIndex] = DBFGetFieldInfo (fromDbfHandle, fieldIndex, fieldName, & fieldWidth, & decimals);

    if (DBFCopyRecord (fieldCount, fieldTypes, fromDbfHandle, toDbfHandle, fromRecordIndex, toRecordIndex)) {
      Tcl_SetResult (interp, "error copying dbf record", TCL_VOLATILE);
      free (fieldTypes);
      return TCL_ERROR;
    } else {
      Tcl_SetResult (interp, "", TCL_VOLATILE);
      free (fieldTypes);
      return TCL_OK;
    }
  }
}

/* Preconditions:
   in/out files are SHPT_ARC
   fieldTypes is dbfFieldCount long, each holding one of (FTString, FTDouble, FTInteger, FTInvalid) */
static int trimArcs (int dbfFieldCount, int * fieldTypes,
		     SHPHandle shpHandleIn, DBFHandle dbfHandleIn, SHPHandle shpHandleOut, DBFHandle dbfHandleOut,
		     double n, double e, double w, double s, int nEntities) {
  int indexIn, indexOut;
  SHPObject * shpObjectIn, * shpObjectOut;
  int vertexCount;
  int bufferSize = 0;
  double * padfX = 0, * padfY = 0;
  int vertex;
  int leftVertexIn, rightVertexIn, lastRightVertexIn = 0;
  int intN, intE, intW, intS;
  double intX, intY,
    intNx, intEx, intWx, intSx,
    intNy, intEy, intWy, intSy;
  int i;

  bufferSize = 100;
  if (! (padfX = (double *) malloc (bufferSize * sizeof (double))))
    return 20;
  if (! (padfY = (double *) malloc (bufferSize * sizeof (double)))) {
    free (padfX);
    return 21;
  }

  for (indexIn = 0; indexIn < nEntities; indexIn ++) {
    shpObjectIn = SHPReadObject (shpHandleIn, indexIn);	/* read the shape */

    if (shpObjectIn -> nParts > 1) {
      free (padfX); free (padfY);
      return 1;
    }

    /* if the whole thing is in the box, copy it out */
    if (pointInBox(shpObjectIn -> dfXMin, shpObjectIn -> dfYMax, n, e, w, s) &&
	pointInBox(shpObjectIn -> dfXMax, shpObjectIn -> dfYMax, n, e, w, s) &&
	pointInBox(shpObjectIn -> dfXMax, shpObjectIn -> dfYMin, n, e, w, s) &&
	pointInBox(shpObjectIn -> dfXMin, shpObjectIn -> dfYMin, n, e, w, s)) {

      /* write the shape */
      if ((indexOut = SHPWriteObject (shpHandleOut, -1, shpObjectIn)) < 0) {
	free (padfX); free (padfY);
	return 2;
      }

      /* copy the dbf record */
      if (DBFCopyRecord (dbfFieldCount, fieldTypes, dbfHandleIn, dbfHandleOut, indexIn, indexOut) != 0) {
	free (padfX); free (padfY);
	return 3;
      }

    } /* else if the clipping rectangle intersects the minimum containing rectangle */
    else if (pointInBox(shpObjectIn -> dfXMin, shpObjectIn -> dfYMax, n, e, w, s) ||
	     pointInBox(shpObjectIn -> dfXMax, shpObjectIn -> dfYMax, n, e, w, s) ||
	     pointInBox(shpObjectIn -> dfXMax, shpObjectIn -> dfYMin, n, e, w, s) ||
	     pointInBox(shpObjectIn -> dfXMin, shpObjectIn -> dfYMin, n, e, w, s)) {

      vertexCount = 0;
      for (vertex = 0; vertex < shpObjectIn -> nVertices - 1; vertex ++) {
	/* see which, if either, ends are in the boundary */
	if (vertex) {		/* not first vertex */
	  leftVertexIn = lastRightVertexIn;
	  rightVertexIn = lastRightVertexIn = pointInBox(shpObjectIn -> padfX [vertex + 1], shpObjectIn -> padfY [vertex + 1], n, e, w, s);
	} else {		/* first vertex */
	  leftVertexIn = pointInBox(shpObjectIn -> padfX [vertex], shpObjectIn -> padfY [vertex], n, e, w, s);
	  rightVertexIn = lastRightVertexIn = pointInBox(shpObjectIn -> padfX [vertex + 1], shpObjectIn -> padfY [vertex + 1], n, e, w, s);
	}

	if (leftVertexIn && rightVertexIn) { /* both ends are in */
	  if (vertexCount) {	/* already created shape out */
	    /* make room for another vertex */
	    vertexCount ++;
	    if (vertexCount > bufferSize) {
	      bufferSize += 100;
	      if (! (padfX = (double *) realloc (padfX, bufferSize * sizeof (double)))) {
		free (padfY);
		return 18;
	      }
	      if (! (padfY = (double *) realloc (padfY, bufferSize * sizeof (double)))) {
		free (padfX);
		return 19;
	      }
	    }

	    /* add right end */
	    padfX [vertexCount - 1] = shpObjectIn -> padfX [vertex + 1];
	    padfY [vertexCount - 1] = shpObjectIn -> padfY [vertex + 1];

	  } else {		/* starting new shape out */
	    /* make new shape */
	    vertexCount = 2;

	    /* add left end */
	    padfX [0] = shpObjectIn -> padfX [vertex];
	    padfY [0] = shpObjectIn -> padfY [vertex];
	    /* add right end */
	    padfX [1] = shpObjectIn -> padfX [vertex + 1];
	    padfY [1] = shpObjectIn -> padfY [vertex + 1];
	  }
	} else if (leftVertexIn ^ rightVertexIn) { /* only one end is in */
	  /* find where this segment intersects the boundary */
	  intN = lines_intersect (shpObjectIn -> padfX [vertex], shpObjectIn -> padfY [vertex],
				  shpObjectIn -> padfX [vertex + 1], shpObjectIn -> padfY [vertex + 1],
				  w, n, e, n,
				  & intX, & intY);
	  intE = lines_intersect (shpObjectIn -> padfX [vertex], shpObjectIn -> padfY [vertex],
				  shpObjectIn -> padfX [vertex + 1], shpObjectIn -> padfY [vertex + 1],
				  e, n, e, s,
				  & intX, & intY);
	  intW = lines_intersect (shpObjectIn -> padfX [vertex], shpObjectIn -> padfY [vertex],
				  shpObjectIn -> padfX [vertex + 1], shpObjectIn -> padfY [vertex + 1],
				  w, n, w, s,
				  & intX, & intY);
	  intS = lines_intersect (shpObjectIn -> padfX [vertex], shpObjectIn -> padfY [vertex],
				  shpObjectIn -> padfX [vertex + 1], shpObjectIn -> padfY [vertex + 1],
				  w, s, e, s,
				  & intX, & intY);
	  if (intN + intE + intW + intS != 1) {
	    /* odd case here; perhaps the segment passes through the clip corner exactly */
	    if ((intN ^ intS) && (intE ^ intW))
	      /* arbitrarily choose N/S boundary */
	      intE = intW = 0;
	    else {
	      printf ("shapeUtils.c:trimArcs: intN %d intE %d intW %d intS %d\n", intN, intE, intW, intS);
	      printf ("shapeUtils.c:trimArcs: x1 %lf y1 %lf x2 %lf y2 %lf\n",
		      shpObjectIn -> padfX [vertex], shpObjectIn -> padfY [vertex],
		      shpObjectIn -> padfX [vertex + 1], shpObjectIn -> padfY [vertex + 1]);
	      free (padfX); free (padfY);
	      return 4;
	    }
	  }

	  if (leftVertexIn) {		/* only the left is in */
	    if (! vertexCount) { /* starting new shape out */
	      /* make new shape */
	      vertexCount = 1;

	      /* add left end */
	      padfX [0] = shpObjectIn -> padfX [vertex];
	      padfY [0] = shpObjectIn -> padfY [vertex];
	    }

	    /* make room for another vertex */
	    vertexCount ++;
	    if (vertexCount > bufferSize) {
	      bufferSize += 100;
	      if (! (padfX = (double *) realloc (padfX, bufferSize * sizeof (double)))) {
		free (padfY);
		return 16;
	      }
	      if (! (padfY = (double *) realloc (padfY, bufferSize * sizeof (double)))) {
		free (padfX);
		return 17;
	      }
	    }

	    /* add intersection (intX, intY) */
	    padfX [vertexCount - 1] = intX;
	    padfY [vertexCount - 1] = intY;

	    /* save the shape */
	    shpObjectOut = SHPCreateSimpleObject (SHPT_ARC, vertexCount, padfX, padfY, 0);
	    if ((indexOut = SHPWriteObject (shpHandleOut, -1, shpObjectOut)) < 0) {
	      free (padfX); free (padfY);
	      return 5;
	    }
	    
	    if (DBFCopyRecord (dbfFieldCount, fieldTypes, dbfHandleIn, dbfHandleOut, indexIn, indexOut) != 0) {
	      free (padfX); free (padfY);
	      return 6;
	    }

	    vertexCount = 0;
	  } else {		/* only the right end is in */
	    if (! vertexCount) { /* starting new shape out */
	      /* make new shape */
	      vertexCount = 2;

	      /* add intersection */
	      padfX [0] = intX;
	      padfY [0] = intY;
	      /* add right end */
	      padfX [1] = shpObjectIn -> padfX [vertex + 1];
	      padfY [1] = shpObjectIn -> padfY [vertex + 1];
	    } else {
	      free (padfX); free (padfY);
	      return 7;
	    }
	  }
	} else {		/* both ends are out, but it may be crossing a corner or crossing the middle (two intersections) */
	  /* find the two intersections (or none) */
	  intN = intE = intW = intS = 0;
	  i = ((intN = lines_intersect (shpObjectIn -> padfX [vertex], shpObjectIn -> padfY [vertex],
					shpObjectIn -> padfX [vertex + 1], shpObjectIn -> padfY [vertex + 1],
					w, n, e, n,
					& intNx, & intNy)) ||
	       (intE = lines_intersect (shpObjectIn -> padfX [vertex], shpObjectIn -> padfY [vertex],
					shpObjectIn -> padfX [vertex + 1], shpObjectIn -> padfY [vertex + 1],
					e, n, e, s,
					& intEx, & intEy)) ||
	       (intW = lines_intersect (shpObjectIn -> padfX [vertex], shpObjectIn -> padfY [vertex],
					shpObjectIn -> padfX [vertex + 1], shpObjectIn -> padfY [vertex + 1],
					w, n, w, s,
					& intWx, & intWy)) ||
	       (intS = lines_intersect (shpObjectIn -> padfX [vertex], shpObjectIn -> padfY [vertex],
					shpObjectIn -> padfX [vertex + 1], shpObjectIn -> padfY [vertex + 1],
					w, s, e, s,
					& intSx, & intSy)));
	  if (intN + intE + intW + intS != 0 &&
	      intN + intE + intW + intS != 2) {
	    free (padfX); free (padfY);
	    return 8;
	  }

	  if (intN == 2 || intE == 2 || intW == 2 || intS == 2) {
	    free (padfX); free (padfY);
	    return 9;
	  }

	  if (intN + intE + intW + intS == 2) {	/* crossing a corner or the middle */
	    if (! vertexCount) { /* starting new shape out */
	      /* make new shape */
	      vertexCount = 2;

	      /* add left and right intersections */
	      if (intN) {
		padfX [0] = intNx;
		padfY [0] = intNy;
	      }
	      if (intE) {
		if (intN) {
		  padfX [1] = intEx;
		  padfY [1] = intEy;
		} else {
		  padfX [0] = intEx;
		  padfY [0] = intEy;
		}
	      }
	      if (intW) {
		if (intN || intE) {
		  padfX [1] = intWx;
		  padfY [1] = intWy;
		} else {
		  padfX [0] = intWx;
		  padfY [0] = intWy;
		}
	      }
	      if (intS) {
		  padfX [1] = intSx;
		  padfY [1] = intSy;
	      }
	      /* we could compare the distances between these and
	      vertex to ensure that [0] has the lesser distance, so
	      that the order of this two vertex arc is consistent with
	      the original arc */
	      
	      /* save shape */
	      shpObjectOut = SHPCreateSimpleObject (SHPT_ARC, vertexCount, padfX, padfY, 0);
	      if ((indexOut = SHPWriteObject (shpHandleOut, -1, shpObjectOut)) < 0) {
		free (padfX); free (padfY);
		return 10;
	      }

	      if (DBFCopyRecord (dbfFieldCount, fieldTypes, dbfHandleIn, dbfHandleOut, indexIn, indexOut) != 0) {
		free (padfX); free (padfY);
		return 11;
	      }

	      vertexCount = 0;

	    } else {
	      free (padfX); free (padfY);
	      return 12;
	    }

	  } else {		/* completely out */
	    if (vertexCount) {	/* already created shape out */
	      free (padfX); free (padfY);
	      return 13;
	    }
	  }
	}
      }
      if (vertexCount) {	/* already created shape out */
	/* save the shape */
	shpObjectOut = SHPCreateSimpleObject (SHPT_ARC, vertexCount, padfX, padfY, 0);
	if ((indexOut = SHPWriteObject (shpHandleOut, -1, shpObjectOut)) < 0) {
	  free (padfX); free (padfY);
	  return 14;
	}

	if (DBFCopyRecord (dbfFieldCount, fieldTypes, dbfHandleIn, dbfHandleOut, indexIn, indexOut) != 0) {
	  free (padfX); free (padfY);
	  return 15;
	}

	vertexCount = 0;
      }
    } /* else do nothing, since the whole shape is outside the box */
  } /* for (indexIn = 0; indexIn < nEntities; indexIn ++) */

  free (padfX); free (padfY);

  return 0;
}

/* Polys with holes are not trimmed; If they touch the clip rect, they are passed through; otherwise not.

   Preconditions:
   in/out files are SHPT_POLYGON
   fieldTypes is dbfFieldCount long, each holding one of (FTString, FTDouble, FTInteger, FTInvalid)
*/
static int trimPolys (int dbfFieldCount, int * fieldTypes,
		     SHPHandle shpHandleIn, DBFHandle dbfHandleIn, SHPHandle shpHandleOut, DBFHandle dbfHandleOut,
		     double n, double e, double w, double s, int nEntities) {
  int indexIn, indexOut;
  SHPObject * shpObjectIn, * shpObjectOut;

  for (indexIn = 0; indexIn < nEntities; indexIn ++) {
    shpObjectIn = SHPReadObject (shpHandleIn, indexIn);	/* read the shape */

    /* if the poly is entirely contained by the clip rect, copy the poly out */
    if ((pointInBox(shpObjectIn -> dfXMin, shpObjectIn -> dfYMax, n, e, w, s) &&
	 pointInBox(shpObjectIn -> dfXMax, shpObjectIn -> dfYMax, n, e, w, s) &&
	 pointInBox(shpObjectIn -> dfXMax, shpObjectIn -> dfYMin, n, e, w, s) &&
	 pointInBox(shpObjectIn -> dfXMin, shpObjectIn -> dfYMin, n, e, w, s))) {

      /* write the shape */
      if ((indexOut = SHPWriteObject (shpHandleOut, -1, shpObjectIn)) < 0)
	return 2;

      /* copy the dbf record */
      if (DBFCopyRecord (dbfFieldCount, fieldTypes, dbfHandleIn, dbfHandleOut, indexIn, indexOut) != 0)
	return 3;

    } else if /* or if it touches the clip rect, use the Sutherland-Hodgman algorithm */
      (! ((shpObjectIn -> dfXMin < w && shpObjectIn -> dfXMax < w) || 
	  (shpObjectIn -> dfXMin > e && shpObjectIn -> dfXMax > e) ||
	  (shpObjectIn -> dfYMin < s && shpObjectIn -> dfYMax < s) ||
	  (shpObjectIn -> dfYMin > n && shpObjectIn -> dfYMax > n))) {

      int i;
      Vertex * head = 0;
      Vertex ** croppedPolys;
      int croppedPolyCount;
      int vertexCount;
      int onePart = 0;
      double * padfX, * padfY;

      /* create vertex list representing the polygon */
      if (shpObjectIn -> nParts > 1) { /* discard the holes */
	for (i = shpObjectIn -> panPartStart [1] - 1; i >= 0; i --)
	  head = Vertex_create (shpObjectIn -> padfX [i], shpObjectIn -> padfY [i], 0, head);
      } else {			/* there aren't any holes */
	for (i = shpObjectIn -> nVertices - 1; i >= 0; i --)
	  head = Vertex_create (shpObjectIn -> padfX [i], shpObjectIn -> padfY [i], 0, head);
      }

      /* crop the polygon */
      cropPolygon (head, & croppedPolys, & croppedPolyCount, n, e, w, s);
      Vertex_destroy (head);

      /* save each resulting polygon */
      for (i = 0; i < croppedPolyCount; i ++) {
	/* count the vertices; copy to arrays; create */
	for (vertexCount = 0, head = croppedPolys [i]; head; vertexCount ++, head = head -> next)
	  ;

	padfX = (double *) malloc (sizeof (double) * vertexCount);
	padfY = (double *) malloc (sizeof (double) * vertexCount);

	for (vertexCount = 0, head = croppedPolys [i]; head; vertexCount ++, head = head -> next) {
	  padfX [vertexCount] = head -> x;
	  padfY [vertexCount] = head -> y;
	}

	shpObjectOut = SHPCreateObject (SHPT_POLYGON, -1, 1, & onePart, 0, vertexCount, padfX, padfY, 0, 0);

	free (padfX);
	free (padfY);

	/* write the shape */
	if ((indexOut = SHPWriteObject (shpHandleOut, -1, shpObjectOut)) < 0)
	  return 4;

	/* copy the dbf record */
	if (DBFCopyRecord (dbfFieldCount, fieldTypes, dbfHandleIn, dbfHandleOut, indexIn, indexOut) != 0)
	  return 5;

	Vertex_destroy (croppedPolys [i]);
	SHPDestroyObject (shpObjectOut);
      }	/* for (i = 0; i < croppedPolyCount; i ++) */
    } /* else use Sutherland-Hodgman algorithm */

    SHPDestroyObject (shpObjectIn);

  } /* for (indexIn = 0; indexIn < nEntities; indexIn ++) */

  return 0;
}

static int trimShapesProc (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv []) {
  char * usage = "wrong # args: should be \"trimShapes shapefileIn shapefileOut n e w s\"";
  char * shapefileIn, * shapefileOut;
  SHPHandle shpHandleIn, shpHandleOut;
  DBFHandle dbfHandleIn, dbfHandleOut;
  int nEntities, nShapetype;
  double xyzmMin [4], xyzmMax [4];
  int fieldIndex;
  int dbfRecordCount, dbfFieldCount;
  int * fieldTypes;
  char ** fieldNames;
  int * fieldWidths;
  int * fieldDecimals;
  int result;
  double n, e, w, s;

  /* check argc */
  if (objc != 7) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  /* check lat/lon parms */
  if (Tcl_GetDoubleFromObj (interp, objv [3], & n) != TCL_OK || n < -90.0 || n > 90.0) {
    Tcl_SetResult (interp, "error parsing n", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetDoubleFromObj (interp, objv [4], & e) != TCL_OK || e < -180.0 || e > 180.0) {
    Tcl_SetResult (interp, "error parsing e", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetDoubleFromObj (interp, objv [5], & w) != TCL_OK || w < -180.0 || w > 180.0) {
    Tcl_SetResult (interp, "error parsing w", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetDoubleFromObj (interp, objv [6], & s) != TCL_OK || s < -90.0 || s > 90.0) {
    Tcl_SetResult (interp, "error parsing s", TCL_VOLATILE);
    return TCL_ERROR;
  }

  /* try to open input files; make sure they're arcs; make sure the shp, dbf match in length */
  shapefileIn = strdup (Tcl_GetStringFromObj (objv [1], 0));

  if (! (shpHandleIn = SHPOpen (shapefileIn, "rb"))) {
    Tcl_SetResult (interp, "error opening input shp", TCL_VOLATILE);
    free (shapefileIn);
    return TCL_ERROR;
  }

  if (! (dbfHandleIn = DBFOpen (shapefileIn, "rb"))) {
    SHPClose (shpHandleIn);
    free (shapefileIn);
    Tcl_SetResult (interp, "error opening input dbf", TCL_VOLATILE);
    return TCL_ERROR;
  }

  free (shapefileIn);

  SHPGetInfo (shpHandleIn, & nEntities, & nShapetype, xyzmMin, xyzmMax);

  if (nShapetype != SHPT_ARC && nShapetype != SHPT_POLYGON) {
    Tcl_SetResult (interp, "shapefileIn not ARC POLYGON vector type", TCL_VOLATILE);
    return TCL_ERROR;
  }

  dbfRecordCount = DBFGetRecordCount (dbfHandleIn);
  dbfFieldCount = DBFGetFieldCount (dbfHandleIn);
  if (dbfRecordCount != nEntities) {
    DBFClose (dbfHandleIn);
    SHPClose (shpHandleIn);
    Tcl_SetResult (interp, "input shp and dbf lengths don't match", TCL_VOLATILE);
    return TCL_ERROR;
  }

  /* try to open output files */
  shapefileOut = strdup (Tcl_GetStringFromObj (objv [2], 0));

  if (! (shpHandleOut = SHPCreate (shapefileOut, nShapetype))) {
    DBFClose (dbfHandleIn);
    SHPClose (shpHandleIn);
    free (shapefileOut);
    Tcl_SetResult (interp, "error creating output shp", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (! (dbfHandleOut = DBFCreate (shapefileOut))) {
    SHPClose (shpHandleOut);
    DBFClose (dbfHandleIn);
    SHPClose (shpHandleIn);
    free (shapefileOut);
    Tcl_SetResult (interp, "error creating output dbf", TCL_VOLATILE);
    return TCL_ERROR;
  }

  free (shapefileOut);
  
  /* get the dbf field info and initialize new dbf file */
  fieldTypes = (int *) malloc (sizeof (int) * dbfFieldCount);
  fieldNames = (char **) malloc (sizeof (char *) * dbfFieldCount);
  fieldWidths = (int *) malloc (sizeof (int) * dbfFieldCount);
  fieldDecimals = (int *) malloc (sizeof (int) * dbfFieldCount);
  for (fieldIndex = 0; fieldIndex < dbfFieldCount; fieldIndex ++) {
    fieldNames [fieldIndex] = malloc (12); /* max field name length (11) plus null */
    fieldTypes [fieldIndex] = DBFGetFieldInfo (dbfHandleIn, fieldIndex, fieldNames [fieldIndex], fieldWidths + fieldIndex, fieldDecimals + fieldIndex);

    switch (fieldTypes [fieldIndex]) {
    case FTString:
    case FTInteger:
    case FTDouble:
      result = DBFAddField (dbfHandleOut, fieldNames [fieldIndex], fieldTypes [fieldIndex], fieldWidths [fieldIndex], fieldDecimals [fieldIndex]);
      break;
    case FTInvalid:
      result = DBFAddField (dbfHandleOut, fieldNames [fieldIndex], FTInteger, 1 /* for a zero value */, 0);
      break;
    default:
      result = -1;
    }

    if (result < 0) {
      DBFClose (dbfHandleOut);
      SHPClose (shpHandleOut);
      DBFClose (dbfHandleIn);
      SHPClose (shpHandleIn);
      /* can't be bothered to free fieldTypes, fieldNames (and referenced strings), fieldWidths, fieldDecimals */
      Tcl_SetResult (interp, "DBFAddField failed", TCL_VOLATILE);
      return TCL_ERROR;
    }
  }

  /* most of the code above should be factored out with DBFCopyBreakingHolesProc */

  /* already verified that it's arc or polygon */
  {
    int status = 0;

    if (nShapetype == SHPT_ARC)
      status = trimArcs (dbfFieldCount, fieldTypes, shpHandleIn, dbfHandleIn, shpHandleOut, dbfHandleOut, n, e, w, s, nEntities);
    else if (nShapetype == SHPT_POLYGON)
      status = trimPolys (dbfFieldCount, fieldTypes, shpHandleIn, dbfHandleIn, shpHandleOut, dbfHandleOut, n, e, w, s, nEntities);

    if (status) {
      char buf [100];
      DBFClose (dbfHandleOut); SHPClose (shpHandleOut); DBFClose (dbfHandleIn); SHPClose (shpHandleIn);
      if (nShapetype == SHPT_ARC)
	sprintf (buf, "trimArcs returned error code %d", status);
      else if (nShapetype == SHPT_POLYGON)
	sprintf (buf, "trimPolys returned error code %d", status);
      Tcl_SetResult (interp, buf, TCL_VOLATILE);
      return TCL_ERROR;
    }
  }

  SHPClose (shpHandleIn);
  DBFClose (dbfHandleIn);
  SHPClose (shpHandleOut);
  DBFClose (dbfHandleOut);

  Tcl_SetResult (interp, "", TCL_VOLATILE);

  return TCL_OK;
}

static int CopyBreakingHolesProc (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv []) {
  char * usage = "wrong # args: should be \"CopyBreakingHoles shapefileIn shapefileOut_noHoles shapefileOut_holes replacedFieldName\"";
  char * shapefileIn, * shapefileOut, *replacedFieldName;
  SHPHandle shpHandleIn, shpHandleOutNh, shpHandleOutH;
  DBFHandle dbfHandleIn, dbfHandleOutNh, dbfHandleOutH;
  int nEntities, nShapetype;
  double xyzmMin [4], xyzmMax [4];
  int indexIn, indexOutNh = 0, indexOutH = 0;
  int fieldIndex, replacedFieldIndex;
  SHPObject * shpObjectIn, * shpObjectOut;
  int dbfRecordCount, dbfFieldCount;
  int * fieldTypes;
  char ** fieldNames;
  int * fieldWidths;
  int * fieldDecimals;
  char * stringValue;
  int part;
  int result1, result2;

  /* check argc */
  if (objc != 5) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  /* try to open input files; make sure they're polygons; make sure the shp, dbf match in length */
  shapefileIn = strdup (Tcl_GetStringFromObj (objv [1], 0));

  if (! (shpHandleIn = SHPOpen (shapefileIn, "rb"))) {
    Tcl_SetResult (interp, "error opening input shp", TCL_VOLATILE);
    free (shapefileIn);
    return TCL_ERROR;
  }

  if (! (dbfHandleIn = DBFOpen (shapefileIn, "rb"))) {
    SHPClose (shpHandleIn);
    free (shapefileIn);
    Tcl_SetResult (interp, "error opening input dbf", TCL_VOLATILE);
    return TCL_ERROR;
  }

  free (shapefileIn);

  SHPGetInfo (shpHandleIn, & nEntities, & nShapetype, xyzmMin, xyzmMax);

  if (nShapetype != SHPT_POLYGON) {
    Tcl_SetResult (interp, "shapefileIn not POLYGON vector type", TCL_VOLATILE);
    return TCL_ERROR;
  }

  dbfRecordCount = DBFGetRecordCount (dbfHandleIn);
  dbfFieldCount = DBFGetFieldCount (dbfHandleIn);
  if (dbfRecordCount != nEntities) {
    DBFClose (dbfHandleIn);
    SHPClose (shpHandleIn);
    Tcl_SetResult (interp, "input shp and dbf lengths don't match", TCL_VOLATILE);
    return TCL_ERROR;
  }

  /* make sure the replaced field exists */
  replacedFieldName = strdup (Tcl_GetStringFromObj (objv [4], 0));
  if ((replacedFieldIndex = DBFGetFieldIndex (dbfHandleIn, replacedFieldName)) < 0) {
    DBFClose (dbfHandleIn);
    SHPClose (shpHandleIn);
    free (replacedFieldName);
    Tcl_SetResult (interp, "named field not found in dbf", TCL_VOLATILE);
    return TCL_ERROR;
  }

  free (replacedFieldName);

  /* try to open output files */
  shapefileOut = strdup (Tcl_GetStringFromObj (objv [2], 0));

  if (! (shpHandleOutNh = SHPCreate (shapefileOut, SHPT_POLYGON))) {
    DBFClose (dbfHandleIn);
    SHPClose (shpHandleIn);
    free (shapefileOut);
    Tcl_SetResult (interp, "error creating outputNh shp", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (! (dbfHandleOutNh = DBFCreate (shapefileOut))) {
    SHPClose (shpHandleOutNh);
    DBFClose (dbfHandleIn);
    SHPClose (shpHandleIn);
    free (shapefileOut);
    Tcl_SetResult (interp, "error creating outputNh dbf", TCL_VOLATILE);
    return TCL_ERROR;
  }

  free (shapefileOut);
  
  shapefileOut = strdup (Tcl_GetStringFromObj (objv [3], 0));

  if (! (shpHandleOutH = SHPCreate (shapefileOut, SHPT_POLYGON))) {
    DBFClose (dbfHandleOutNh);
    SHPClose (shpHandleOutNh);
    DBFClose (dbfHandleIn);
    SHPClose (shpHandleIn);
    free (shapefileOut);
    Tcl_SetResult (interp, "error creating outputH shp", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (! (dbfHandleOutH = DBFCreate (shapefileOut))) {
    DBFClose (dbfHandleOutNh);
    SHPClose (shpHandleOutNh);
    SHPClose (shpHandleOutH);
    DBFClose (dbfHandleIn);
    SHPClose (shpHandleIn);
    free (shapefileOut);
    Tcl_SetResult (interp, "error creating outputH dbf", TCL_VOLATILE);
    return TCL_ERROR;
  }

  free (shapefileOut);
  
  /* get the dbf field info and initialize new dbf file */
  fieldTypes = (int *) malloc (sizeof (int) * dbfFieldCount);
  fieldNames = (char **) malloc (sizeof (char *) * dbfFieldCount);
  fieldWidths = (int *) malloc (sizeof (int) * dbfFieldCount);
  fieldDecimals = (int *) malloc (sizeof (int) * dbfFieldCount);
  for (fieldIndex = 0; fieldIndex < dbfFieldCount; fieldIndex ++) {
    fieldNames [fieldIndex] = malloc (12); /* max field name length (11) plus null */
    fieldTypes [fieldIndex] = DBFGetFieldInfo (dbfHandleIn, fieldIndex, fieldNames [fieldIndex], fieldWidths + fieldIndex, fieldDecimals + fieldIndex);

    switch (fieldTypes [fieldIndex]) {
    case FTString:
    case FTInteger:
    case FTDouble:
      result1 = DBFAddField (dbfHandleOutNh, fieldNames [fieldIndex], fieldTypes [fieldIndex], fieldWidths [fieldIndex], fieldDecimals [fieldIndex]);
      result2 = DBFAddField (dbfHandleOutH, fieldNames [fieldIndex], fieldTypes [fieldIndex], fieldWidths [fieldIndex], fieldDecimals [fieldIndex]);
      break;
    case FTInvalid:
      result1 = DBFAddField (dbfHandleOutNh, fieldNames [fieldIndex], FTInteger, 1 /* for a zero value */, 0);
      result2 = DBFAddField (dbfHandleOutH, fieldNames [fieldIndex], FTInteger, 1 /* for a zero value */, 0);
      break;
    default:
      result1 = result2 = -1;
    }

    if (result1 < 0 || result2 < 0) {
      DBFClose (dbfHandleOutNh);
      SHPClose (shpHandleOutNh);
      DBFClose (dbfHandleOutH);
      SHPClose (shpHandleOutH);
      DBFClose (dbfHandleIn);
      SHPClose (shpHandleIn);
      /* can't be bothered to free fieldTypes, fieldNames (and referenced strings), fieldWidths, fieldDecimals */
      Tcl_SetResult (interp, "DBFAddField failed", TCL_VOLATILE);
      return TCL_ERROR;
    }
  }

  /* make sure replaced field is a string */
  if (fieldTypes [replacedFieldIndex] != FTString) {
    DBFClose (dbfHandleOutNh); SHPClose (shpHandleOutNh); DBFClose (dbfHandleOutH); SHPClose (shpHandleOutH);
    DBFClose (dbfHandleIn); SHPClose (shpHandleIn);
    /* can't be bothered to free fieldTypes, fieldNames (and referenced strings), fieldWidths, fieldDecimals */
    Tcl_SetResult (interp, "replaced field is not an FTString", TCL_VOLATILE);
    return TCL_ERROR;
  }

  /* foreach entity */
  for (indexIn = 0; indexIn < nEntities; indexIn ++) {
    /* read the shape */
    shpObjectIn = SHPReadObject (shpHandleIn, indexIn);

    /* if it has no holes, copy it out to Nh */
    if (shpObjectIn -> nParts == 1) {
      /* write the shape */
      if ((indexOutNh = SHPWriteObject (shpHandleOutNh, -1, shpObjectIn)) < 0) {
	DBFClose (dbfHandleOutNh); SHPClose (shpHandleOutNh); DBFClose (dbfHandleOutH); SHPClose (shpHandleOutH);
	DBFClose (dbfHandleIn); SHPClose (shpHandleIn);
	/* can't be bothered to free fieldTypes, fieldNames (and referenced strings), fieldWidths, fieldDecimals */
	Tcl_SetResult (interp, "SHPWriteObject failed", TCL_VOLATILE);
	return TCL_ERROR;
      }

      /* copy the dbf record */
      if (DBFCopyRecord (dbfFieldCount, fieldTypes, dbfHandleIn, dbfHandleOutNh, indexIn, indexOutNh) != 0) {
	DBFClose (dbfHandleOutNh); SHPClose (shpHandleOutNh); DBFClose (dbfHandleOutH); SHPClose (shpHandleOutH);
	DBFClose (dbfHandleIn); SHPClose (shpHandleIn);
	/* can't be bothered to free fieldTypes, fieldNames (and referenced strings), fieldWidths, fieldDecimals, stringValue */
	Tcl_SetResult (interp, "DBFCopyRecord failed", TCL_VOLATILE);
	return TCL_ERROR;
      }

    } else {
      /* else if it has holes, copy each part out to Nh, changing fcode of second and later parts, writing to H */

      int nVertices;

      /* for each part */
      for (part = 0; part < shpObjectIn -> nParts; part ++) {
	/* determine part vertex count */
	if (part < shpObjectIn -> nParts - 1) /* if it's not the last part */
	  nVertices = shpObjectIn -> panPartStart [part + 1] - shpObjectIn -> panPartStart [part];
	else			/* if it is the last part */
	  nVertices = shpObjectIn -> nVertices - shpObjectIn -> panPartStart [part];

	/* create new shape for part, copying from shpObjectIn */
	shpObjectOut = SHPCreateSimpleObject (shpObjectIn -> nSHPType,
					      nVertices,
					      shpObjectIn -> padfX + shpObjectIn -> panPartStart [part],
					      shpObjectIn -> padfY + shpObjectIn -> panPartStart [part],
					      shpObjectIn -> padfZ + shpObjectIn -> panPartStart [part]);

	/* write the new shape */
	if (part) {
	  if ((indexOutH = SHPWriteObject (shpHandleOutH, -1, shpObjectOut)) < 0) {
	    DBFClose (dbfHandleOutNh); SHPClose (shpHandleOutNh); DBFClose (dbfHandleOutH); SHPClose (shpHandleOutH);
	    DBFClose (dbfHandleIn); SHPClose (shpHandleIn);
	    /* can't be bothered to free fieldTypes, fieldNames (and referenced strings), fieldWidths, fieldDecimals */
	    Tcl_SetResult (interp, "SHPWriteObject failed", TCL_VOLATILE);
	    return TCL_ERROR;
	  }
	} else {
	  if ((indexOutNh = SHPWriteObject (shpHandleOutNh, -1, shpObjectOut)) < 0) {
	    DBFClose (dbfHandleOutNh); SHPClose (shpHandleOutNh); DBFClose (dbfHandleOutH); SHPClose (shpHandleOutH);
	    DBFClose (dbfHandleIn); SHPClose (shpHandleIn);
	    /* can't be bothered to free fieldTypes, fieldNames (and referenced strings), fieldWidths, fieldDecimals */
	    Tcl_SetResult (interp, "SHPWriteObject failed", TCL_VOLATILE);
	    return TCL_ERROR;
	  }
	}

	/* discard the new shape */
	SHPDestroyObject (shpObjectOut);

	/* this should be factored against the if of this else */
	/* write the dbf record */
	for (fieldIndex = 0; fieldIndex < dbfFieldCount; fieldIndex ++) {
	  switch (fieldTypes [fieldIndex]) {	/* we already verified one of these four types */
	  case FTString:
	    /* if this is a hole and the field to replace, use the new value instead of the current value */
	    stringValue = (char *) DBFReadStringAttribute (dbfHandleIn, indexIn, fieldIndex);
	    if (stringValue) {
	      stringValue = strdup (stringValue);
	    } else {
	      stringValue = strdup ("");
	    }

#define suffix " Hole"
	    if (part && fieldIndex == replacedFieldIndex) {
	      stringValue = realloc (stringValue, strlen(stringValue) + strlen(suffix) + 1);
	      strcat (stringValue, suffix);
	    }

	    if (! DBFWriteStringAttribute (part?dbfHandleOutH:dbfHandleOutNh, part?indexOutH:indexOutNh, fieldIndex, stringValue)) {
	      DBFClose (dbfHandleOutNh); SHPClose (shpHandleOutNh); DBFClose (dbfHandleOutH); SHPClose (shpHandleOutH);
	      DBFClose (dbfHandleIn); SHPClose (shpHandleIn);
	      /* can't be bothered to free fieldTypes, fieldNames (and referenced strings), fieldWidths, fieldDecimals, stringValue */
	      Tcl_SetResult (interp, "DBFWriteStringAttribute failed", TCL_VOLATILE);
	      return TCL_ERROR;
	    }
	    free (stringValue);
	    break;

	  case FTDouble:
	    if (! DBFWriteDoubleAttribute
		(part?dbfHandleOutH:dbfHandleOutNh, part?indexOutH:indexOutNh, fieldIndex, DBFReadDoubleAttribute (dbfHandleIn, indexIn, fieldIndex))) {
	      DBFClose (dbfHandleOutNh); SHPClose (shpHandleOutNh); DBFClose (dbfHandleOutH); SHPClose (shpHandleOutH);
	      DBFClose (dbfHandleIn); SHPClose (shpHandleIn);
	      /* can't be bothered to free fieldTypes, fieldNames (and referenced strings), fieldWidths, fieldDecimals */
	      Tcl_SetResult (interp, "DBFWriteDoubleAttribute failed", TCL_VOLATILE);
	      return TCL_ERROR;
	    }
	    break;

	  case FTInteger:
	    if (! DBFWriteIntegerAttribute
		(part?dbfHandleOutH:dbfHandleOutNh, part?indexOutH:indexOutNh, fieldIndex, DBFReadIntegerAttribute (dbfHandleIn, indexIn, fieldIndex))) {
	      DBFClose (dbfHandleOutNh); SHPClose (shpHandleOutNh); DBFClose (dbfHandleOutH); SHPClose (shpHandleOutH);
	      DBFClose (dbfHandleIn); SHPClose (shpHandleIn);
	      /* can't be bothered to free fieldTypes, fieldNames (and referenced strings), fieldWidths, fieldDecimals */
	      Tcl_SetResult (interp, "DBFWriteIntegerAttribute failed", TCL_VOLATILE);
	      return TCL_ERROR;
	    }
	    break;

	  case FTInvalid:
	    if (! DBFWriteIntegerAttribute (part?dbfHandleOutH:dbfHandleOutNh, part?indexOutH:indexOutNh, fieldIndex, 0)) {
	      DBFClose (dbfHandleOutNh); SHPClose (shpHandleOutNh); DBFClose (dbfHandleOutH); SHPClose (shpHandleOutH);
	      DBFClose (dbfHandleIn); SHPClose (shpHandleIn);
	      /* can't be bothered to free fieldTypes, fieldNames (and referenced strings), fieldWidths, fieldDecimals */
	      Tcl_SetResult (interp, "DBFWriteIntegerAttribute failed", TCL_VOLATILE);
	      return TCL_ERROR;
	    }
	  } /* switch (fieldTypes [fieldIndex]) */
	} /* for (fieldIndex = 0; fieldIndex < dbfFieldCount; fieldIndex ++) */
      } /*       for (part = 0; part < shpObjectIn -> nParts; part ++) */
    } /* if (shpObjectIn -> nParts > 1) */

    /* discard the old shape */
    SHPDestroyObject (shpObjectIn);
  } /* for (indexIn = 0; indexIn < nEntities; indexIn ++) */

  DBFClose (dbfHandleOutNh); SHPClose (shpHandleOutNh); DBFClose (dbfHandleOutH); SHPClose (shpHandleOutH);
  DBFClose (dbfHandleIn); SHPClose (shpHandleIn);

  return TCL_OK;
}

static int DBFCopyStringProc (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv []) {
  char * usage = "wrong # args: should be \"DBFCopyString fromDbfHandle fromShapeIndex fromFieldIndex toDbfHandle toShapeIndex toFieldIndex\"";
  long longDbfHandle;
  DBFHandle fromDbfHandle, toDbfHandle;
  int fromShapeIndex, fromFieldIndex;
  int toShapeIndex, toFieldIndex;
  char * value;

  if (objc != 7) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetLongFromObj (interp, objv [1], & longDbfHandle) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing fromDbfHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  fromDbfHandle = (DBFHandle) longDbfHandle;

  if (Tcl_GetLongFromObj (interp, objv [4], & longDbfHandle) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing toDbfHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  toDbfHandle = (DBFHandle) longDbfHandle;

  if (Tcl_GetIntFromObj (interp, objv [2], & fromShapeIndex) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing fromShapeIndex", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetIntFromObj (interp, objv [5], & toShapeIndex) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing toShapeIndex", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetIntFromObj (interp, objv [3], & fromFieldIndex) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing fromFieldIndex", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetIntFromObj (interp, objv [6], & toFieldIndex) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing toFieldIndex", TCL_VOLATILE);
    return TCL_ERROR;
  }

  value = (char *) DBFReadStringAttribute (fromDbfHandle, fromShapeIndex, fromFieldIndex);

  if (value) {
    value = strdup (value);
  } else {
    value = strdup ("");
  }

  if (! DBFWriteStringAttribute (toDbfHandle, toShapeIndex, toFieldIndex, value)) {
    Tcl_SetResult (interp, "DBFWriteStringAttribute failed", TCL_VOLATILE);
    free (value);
    return TCL_ERROR;
  }

  free (value);

  Tcl_SetResult (interp, "", TCL_VOLATILE);

  return TCL_OK;
}

static int DBFReadStringAttributeProc (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv []) {
  char * usage = "wrong # args: should be \"DBFReadStringAttribute dbfHandle dbfIndex fieldIndex\"";
  long longDbfHandle;
  DBFHandle dbfHandle;
  int dbfIndex, fieldIndex;
  char * value;

  if (objc != 4) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetLongFromObj (interp, objv [1], & longDbfHandle) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing fromDbfHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  dbfHandle = (DBFHandle) longDbfHandle;

  if (Tcl_GetIntFromObj (interp, objv [2], & dbfIndex) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing dbfIndex", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetIntFromObj (interp, objv [3], & fieldIndex) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing fieldIndex", TCL_VOLATILE);
    return TCL_ERROR;
  }

  value = (char *) DBFReadStringAttribute (dbfHandle, dbfIndex, fieldIndex);

  if (value) {
    value = strdup (value);
  } else {
    value = strdup ("");
  }

  Tcl_SetResult (interp, value, TCL_VOLATILE);

  free (value);

  return TCL_OK;
}

static int DBFCloseProc (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv []) {
  char * usage = "wrong # args: should be \"DBFClose dbfHandle\"";
  long longDbfHandle;
  DBFHandle dbfHandle;

  if (objc != 2) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetLongFromObj (interp, objv [1], & longDbfHandle) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing dbfHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  dbfHandle = (DBFHandle) longDbfHandle;

  DBFClose (dbfHandle);

  Tcl_SetResult (interp, "", TCL_VOLATILE);

  return TCL_OK;
}

static int SHPCloseProc (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv []) {
  char * usage = "wrong # args: should be \"SHPClose shpHandle\"";
  long longShpHandle;
  SHPHandle shpHandle;

  if (objc != 2) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetLongFromObj (interp, objv [1], & longShpHandle) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing shpHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  shpHandle = (SHPHandle) longShpHandle;

  SHPClose (shpHandle);

  Tcl_SetResult (interp, "", TCL_VOLATILE);

  return TCL_OK;
}

static int SHPWriteSimpleObjectProc (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv []) {
  char * usage = "wrong # args: should be \"SHPWriteSimpleObject shpHandle shpIndex vertices scalePower\"";
  long longShpHandle;
  int vertexCount = 0;
  double * padfX = 0, * padfY = 0, * padfZ = 0;
  char * vertices, * p;
  double x, y;
  SHPHandle shpHandle;
  int nEntities, nShapetype;
  double xyzmMin [4], xyzmMax [4];
  int minVertexCount;
  int scalePower;
  SHPObject * shapeObject;
  int returnValue;
  int shpIndex;

  if (objc != 5) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetIntFromObj (interp, objv [4], & scalePower) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing scalePower", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetLongFromObj (interp, objv [1], & longShpHandle) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing shpHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  shpHandle = (SHPHandle) longShpHandle;

  if (Tcl_GetIntFromObj (interp, objv [2], & shpIndex) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing shpIndex", TCL_VOLATILE);
    return TCL_ERROR;
  }

  SHPGetInfo (shpHandle, & nEntities, & nShapetype, xyzmMin, xyzmMax);

  switch (nShapetype) {
    case SHPT_ARC:
      minVertexCount = 2;
      break;
    case SHPT_POLYGON:
      minVertexCount = 3;
      break;
  default:
    Tcl_SetResult (interp, "unsupported shape type", TCL_VOLATILE);
    return TCL_ERROR;
  }

  vertices = strdup (Tcl_GetStringFromObj (objv [3], 0));

  p = vertices;

  while (* p) {
    if (sscanf (p, "%lf %lf", & x, & y) == 2) {
      vertexCount ++;
      padfX = (double *) realloc (padfX, sizeof (double) * vertexCount);
      padfX [vertexCount - 1] = x / pow (2.0, scalePower);
      padfY = (double *) realloc (padfY, sizeof (double) * vertexCount);
      padfY [vertexCount - 1] = - y / pow (2.0, scalePower);
    } else
      break;

    while (* p == ' ')		/* skip any space before pair of doubles */
      p ++;
    while (* p && * p != ' ')	/* skip first double */
      p ++;
    while (* p == ' ')		/* skip any space between pair of doubles */
      p ++;
    while (* p && * p != ' ')	/* skip second double */
      p ++;
  }

  if (vertexCount < minVertexCount) {
    free (vertices);
    free (padfX);
    free (padfY);

    Tcl_SetResult (interp, "not enough vertices for shape type", TCL_VOLATILE);
    return TCL_ERROR;
  }

  padfZ = (double *) malloc (sizeof (double) * vertexCount);
  memset (padfZ, vertexCount, sizeof (double));
  
  shapeObject = SHPCreateSimpleObject (nShapetype, vertexCount, padfX, padfY, padfZ);

  {
    int id = SHPWriteObject (shpHandle, shpIndex, shapeObject);

    if (id < 0) {
      Tcl_SetResult (interp, "SHPWriteObject failed", TCL_VOLATILE);
      returnValue = TCL_ERROR;
    } else {
      Tcl_SetObjResult (interp, Tcl_NewIntObj (id));
      returnValue = TCL_OK;
    }
  }

  SHPDestroyObject (shapeObject);

  free (vertices);
  free (padfX);
  free (padfY);
  free (padfZ);

  return returnValue;
}

static int utmToLatLonProc (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv []) {
  char * usage = "wrong # args: should be \"utmToLatLon northing easting zone hemisphere\"";
  double northing, easting;
  int zone;
  char * hemisphere;

  if (objc != 5) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetDoubleFromObj (interp, objv [1], & northing) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing northing", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetDoubleFromObj (interp, objv [2], & easting) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing easting", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetIntFromObj (interp, objv [3], & zone) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing zone", TCL_VOLATILE);
    return TCL_ERROR;
  }

  hemisphere = strdup (Tcl_GetStringFromObj (objv [4], 0));

  {
    int status;
    double lat, lon;
    double a = 6378137.000, f = 1.0 / 298.257223563;
    double Central_Meridian;
    double pi = 3.14159265358979323;
    double degrad = 180.0/pi;
    Tcl_Obj * twoDoubleObjs [2];

    Central_Meridian = (zone - 1) * 6 - 180 + 3;
    Set_UTM_Parameters(a, f, 0);
    Set_Transverse_Mercator_Parameters(a, f, 0.0, Central_Meridian, 500000.0, 10000.0, 1.0);

    status = Convert_UTM_To_Geodetic(zone, * hemisphere, easting, northing, & lat , & lon);

    if (status == UTM_NO_ERROR) {
      twoDoubleObjs [0] = Tcl_NewDoubleObj (lat * degrad);
      twoDoubleObjs [1] = Tcl_NewDoubleObj (lon * degrad);
      Tcl_SetObjResult (interp, Tcl_NewListObj (2, twoDoubleObjs));
      return TCL_OK;
    } else {
      Tcl_SetResult (interp, "Convert_UTM_To_Geodetic returned error code", TCL_VOLATILE);
      return TCL_ERROR;
    }
  }  
}

static int SHPCopyVectorProc (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv []) {
  char * usage = "wrong # args: should be \"SHPCopyVector fromShpHandle fromShapeIndex toShpHandle\"";
  long longShpHandle;
  SHPHandle fromShpHandle, toShpHandle;
  int fromShapeIndex;
  SHPObject * obj;

  if (objc != 4) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetLongFromObj (interp, objv [1], & longShpHandle) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing fromShpHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  fromShpHandle = (SHPHandle) longShpHandle;

  if (Tcl_GetLongFromObj (interp, objv [3], & longShpHandle) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing toShpHandle", TCL_VOLATILE);
    return TCL_ERROR;
  }
  toShpHandle = (SHPHandle) longShpHandle;

  if (Tcl_GetIntFromObj (interp, objv [2], & fromShapeIndex) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing fromShapeIndex", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (! (obj = SHPReadObject (fromShpHandle, fromShapeIndex))) {
    char buf[100];
    sprintf (buf, "SHPReadObject failed for shpHandle %ld, shpIndex %d", (long) fromShpHandle, fromShapeIndex);
    Tcl_SetResult (interp, buf, TCL_VOLATILE);
    return TCL_ERROR;
  }

  {
    int id = SHPWriteObject (toShpHandle, -1, obj);

    if (id < 0) {
      Tcl_SetResult (interp, "SHPWriteObject failed", TCL_VOLATILE);
      return TCL_ERROR;
    }
  }

  Tcl_SetResult (interp, "", TCL_VOLATILE);

  return TCL_OK;
}


static int getUniqueStringFieldValues (DBFHandle dbfHandle, int fieldIndex, Tcl_Obj *** uniqueFields, int maxValues) {
  int dbfRecordCount = DBFGetRecordCount (dbfHandle);
  Tcl_Obj ** fields = 0;
  int count = 0;
  int vecIndex;
  const char * value;
  int listIndex;
  char ** list = 0;

  /* collect the unique field values */
  for (vecIndex = 0; vecIndex < dbfRecordCount; vecIndex ++) {
    /* get this record's value */
    value = DBFReadStringAttribute (dbfHandle, vecIndex, fieldIndex);

    /* look for it in the list */
    for (listIndex = 0; listIndex < count; listIndex ++)
      if (! strcmp (value, list [listIndex]))
	break;

    /* if it's not in the list, add it */
    if (! count || listIndex == count) {
      count ++;
      list = (char **) realloc (list, sizeof (char *) * count);
      list [count - 1] = strdup (value);
    }

    if (count == maxValues)
      break;
  }

  /* create Tcl_Obj list */
  fields = (Tcl_Obj **) malloc (count * sizeof (Tcl_Obj **));
  for (listIndex = 0; listIndex < count; listIndex ++) {
    fields [listIndex] = Tcl_NewStringObj (list [listIndex], -1);
    free (list [listIndex]);
  }

  free (list);

  * uniqueFields = fields;
  return count;
}

static int getUniqueIntegerFieldValues (DBFHandle dbfHandle, int fieldIndex, Tcl_Obj *** uniqueFields, int maxValues) {
  int dbfRecordCount = DBFGetRecordCount (dbfHandle);
  Tcl_Obj ** fields = 0;
  int count = 0;
  int vecIndex;
  int value;
  int listIndex;
  int * list = 0;

  /* collect the unique field values */
  for (vecIndex = 0; vecIndex < dbfRecordCount; vecIndex ++) {
    /* get this record's value */
    value = DBFReadIntegerAttribute (dbfHandle, vecIndex, fieldIndex);

    /* look for it in the list */
    for (listIndex = 0; listIndex < count; listIndex ++)
      if (value == list [listIndex])
	break;

    /* if it's not in the list, add it */
    if (! count || listIndex == count) {
      count ++;
      list = (int *) realloc (list, sizeof (int) * count);
      list [count - 1] = value;
    }

    if (count == maxValues)
      break;
  }

  /* create Tcl_Obj list */
  fields = (Tcl_Obj **) malloc (count * sizeof (Tcl_Obj **));
  for (listIndex = 0; listIndex < count; listIndex ++)
    fields [listIndex] = Tcl_NewIntObj (list [listIndex]);

  free (list);

  * uniqueFields = fields;
  return count;
}

static int getUniqueDoubleFieldValues (DBFHandle dbfHandle, int fieldIndex, Tcl_Obj *** uniqueFields, int maxValues) {
  int dbfRecordCount = DBFGetRecordCount (dbfHandle);
  Tcl_Obj ** fields = 0;
  int count = 0;
  int vecIndex;
  double value;
  int listIndex;
  double * list = 0;

  /* collect the unique field values */
  for (vecIndex = 0; vecIndex < dbfRecordCount; vecIndex ++) {
    /* get this record's value */
    value = DBFReadDoubleAttribute (dbfHandle, vecIndex, fieldIndex);

    /* look for it in the list */
    for (listIndex = 0; listIndex < count; listIndex ++)
      if (value == list [listIndex])
	break;

    /* if it's not in the list, add it */
    if (! count || listIndex == count) {
      count ++;
      list = (double *) realloc (list, sizeof (double) * count);
      list [count - 1] = value;
    }

    if (count == maxValues)
      break;
  }

  /* create Tcl_Obj list */
  fields = (Tcl_Obj **) malloc (count * sizeof (Tcl_Obj **));
  for (listIndex = 0; listIndex < count; listIndex ++)
    fields [listIndex] = Tcl_NewDoubleObj (list [listIndex]);

  free (list);

  * uniqueFields = fields;
  return count;
}

static int getAttributeValueSummary (ClientData clientData, Tcl_Interp *interp, int argc, CONST char * argv []) {
  char * usage = "wrong # args: should be \"getAttributeValueSummary shapefile maxValues ?stringOnlyBit?\"";
  DBFHandle dbfHandle;
  int dbfFieldCount;
  Tcl_Obj ** returnValue, * returnValueList, ** returnedList;
  Tcl_Obj * objPair [2];
  int realFieldCount, realField;
  int field;
  DBFFieldType fieldType;
  char fieldName [13];
  int fieldWidth;
  int decimals;
  int returnedListLength;
  int maxValues;
  int stringOnlyBit = 1;

  if (argc < 3) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (argc == 4 && ! strcmp (argv [3], "0"))
    stringOnlyBit = 0;

  if (sscanf (argv [2], "%d", & maxValues) != 1 || maxValues < 1) {
    Tcl_SetResult (interp, "error parsing positive integer from maxValues", TCL_VOLATILE);
    return TCL_ERROR;
  }

  dbfHandle = DBFOpen (argv [1], "rb");

  if (! dbfHandle) {
    Tcl_SetResult (interp, "DBFOpen failed", TCL_VOLATILE);
    return TCL_ERROR;
  }

  dbfFieldCount = DBFGetFieldCount (dbfHandle);

  /* count the number of fields with supported types (string, integer, double) */
  for (field = realFieldCount = 0; field < dbfFieldCount; field ++) {
    fieldType = DBFGetFieldInfo (dbfHandle, field, fieldName, & fieldWidth, & decimals);
    switch (fieldType) {
    case FTString:
      realFieldCount ++;
      break;
    case FTInteger:
    case FTDouble:
      if (! stringOnlyBit)
	realFieldCount ++;
      break;
    default:
      ; /* ignore uknown field types */
    }
  }

  returnValue = (Tcl_Obj **) malloc (sizeof (Tcl_Obj *) * realFieldCount); /* name and value for each field */

  for (field = realField = 0; field < dbfFieldCount; field ++, realField ++) {
    fieldType = DBFGetFieldInfo (dbfHandle, field, fieldName, & fieldWidth, & decimals);
    switch (fieldType) {
    case FTString:
      objPair [0] = Tcl_NewStringObj (fieldName, -1);
      returnedListLength = getUniqueStringFieldValues (dbfHandle, field, & returnedList, maxValues);
      objPair [1] = Tcl_NewListObj (returnedListLength, returnedList);
      free (returnedList);
      returnValue [realField] = Tcl_NewListObj (2, objPair);
      break;
    case FTInteger:
      if (stringOnlyBit)
	realField --;
      else {
	objPair [0] = Tcl_NewStringObj (fieldName, -1);
	returnedListLength = getUniqueIntegerFieldValues (dbfHandle, field, & returnedList, maxValues);
	objPair [1] = Tcl_NewListObj (returnedListLength, returnedList);
	free (returnedList);
	returnValue [realField] = Tcl_NewListObj (2, objPair);
      }
      break;
    case FTDouble:
      if (stringOnlyBit)
	realField --;
      else {
	objPair [0] = Tcl_NewStringObj (fieldName, -1);
	returnedListLength = getUniqueDoubleFieldValues (dbfHandle, field, & returnedList, maxValues);
	objPair [1] = Tcl_NewListObj (returnedListLength, returnedList);
	free (returnedList);
	returnValue [realField] = Tcl_NewListObj (2, objPair);
      }
      break;
    default:
      realField --;
    }
  }

  DBFClose (dbfHandle);

  returnValueList = Tcl_NewListObj (realFieldCount, returnValue);
  free (returnValue);
  Tcl_SetObjResult (interp, returnValueList);

  return TCL_OK;
}

static const void* cnstsP;

static int mgrsToLatLonZoneXYProc (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv []) {
  char * usage = "wrong # args: should be \"mgrsToLatLonZoneXY mgrsString\"";
  char * mgrs, * mgrss;
  double lat = 0.0, lon = 0.0, x, y;
  long zone;
  Tcl_Obj * objPair [5];

  /* check argc */
  if (objc != 2) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  mgrs = strdup (Tcl_GetStringFromObj (objv [1], 0));
  mgrss = malloc (strlen (mgrs) + 2);
  sprintf (mgrss, "%s ", mgrs);	/* mgrs_to_geo needs trailing space */
  free (mgrs);

  if (mgrs_to_geo (cnstsP, mgrss, & lat, & lon, & zone, & x, & y)) {
    Tcl_SetResult (interp, "mgrs_to_geo failed", TCL_VOLATILE);
    free (mgrss);
    return TCL_ERROR;
  }
  
  free (mgrss);

  objPair [0] = Tcl_NewDoubleObj (lat);
  objPair [1] = Tcl_NewDoubleObj (lon);
  objPair [2] = Tcl_NewIntObj (zone);
  objPair [3] = Tcl_NewDoubleObj (x);
  objPair [4] = Tcl_NewDoubleObj (y);

  Tcl_SetObjResult (interp, Tcl_NewListObj (5, objPair));

  return TCL_OK;
}

static int latLonToMgrsZoneXYProc (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv []) {
  char * usage = "wrong # args: should be \"latLonToMgrsZoneXY lat lon\"";
  char mgrs [50];
  double lat, lon, x, y;
  long zone;
  Tcl_Obj * objList [4];

  /* check argc */
  if (objc != 3) {
    Tcl_SetResult (interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_GetDoubleFromObj (interp, objv [1], & lat) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing lat", TCL_VOLATILE);
    return TCL_ERROR;
  }
  if (Tcl_GetDoubleFromObj (interp, objv [2], & lon) != TCL_OK) {
    Tcl_SetResult (interp, "error parsing lon", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (geo_to_mgrs (cnstsP, lat, lon, mgrs, & zone, & x, & y)) {
    Tcl_SetResult (interp, "geo_to_mgrs failed", TCL_VOLATILE);
    return TCL_ERROR;
  }
  
  objList [0] = Tcl_NewStringObj (mgrs, -1);
  objList [1] = Tcl_NewIntObj (zone);
  objList [2] = Tcl_NewDoubleObj (x);
  objList [3] = Tcl_NewDoubleObj (y);

  Tcl_SetObjResult (interp, Tcl_NewListObj (4, objList));

  return TCL_OK;
}

int Shapeutils_Init (Tcl_Interp *interp) {
  double wgs84[] = { 6378137.000, 298.257223563 };
  cnstsP = set_mgrs (wgs84[0], wgs84[1]);

  Tcl_CreateCommand(interp, "DBFCreate", DBFCreateProc, 0, 0);
  Tcl_CreateCommand(interp, "DBFOpen", DBFOpenProc, 0, 0);
  Tcl_CreateCommand(interp, "SHPCreate", SHPCreateProc, 0, 0);
  Tcl_CreateCommand(interp, "SHPOpen", SHPOpenProc, 0, 0);
  Tcl_CreateCommand(interp, "getAttributeValueSummary", getAttributeValueSummary, 0, 0);
  Tcl_CreateCommand(interp, "getShapeFile", getShapeFileProc, 0, 0);
  Tcl_CreateCommand(interp, "getVectorProperties", getVectorPropertiesProc, 0, 0);
  Tcl_CreateCommand(interp, "getVectorPropertiesFromOpenFile", getVectorPropertiesFromOpenFileProc, 0, 0);
  Tcl_CreateObjCommand(interp, "CopyBreakingHoles", CopyBreakingHolesProc, 0, 0);
  Tcl_CreateObjCommand(interp, "DBFAddField", DBFAddFieldProc, 0, 0);
  Tcl_CreateObjCommand(interp, "DBFClose", DBFCloseProc, 0, 0);
  Tcl_CreateObjCommand(interp, "DBFCopyDouble", DBFCopyDoubleProc, 0, 0);
  Tcl_CreateObjCommand(interp, "DBFCopyInteger", DBFCopyIntegerProc, 0, 0);
  Tcl_CreateObjCommand(interp, "DBFCopyRecord", DBFCopyRecordProc, 0, 0);
  Tcl_CreateObjCommand(interp, "DBFCopySchema", DBFCopySchemaProc, 0, 0);
  Tcl_CreateObjCommand(interp, "DBFCopyString", DBFCopyStringProc, 0, 0);
  Tcl_CreateObjCommand(interp, "DBFGetInfo", DBFGetInfoProc, 0, 0);
  Tcl_CreateObjCommand(interp, "DBFGetRecordCount", DBFGetRecordCountProc, 0, 0);
  Tcl_CreateObjCommand(interp, "DBFReadStringAttribute", DBFReadStringAttributeProc, 0, 0);
  Tcl_CreateObjCommand(interp, "DBFWriteDouble", DBFWriteDoubleProc, 0, 0);
  Tcl_CreateObjCommand(interp, "DBFWriteInteger", DBFWriteIntegerProc, 0, 0);
  Tcl_CreateObjCommand(interp, "DBFWriteString", DBFWriteStringProc, 0, 0);
  Tcl_CreateObjCommand(interp, "SHPClose", SHPCloseProc, 0, 0);
  Tcl_CreateObjCommand(interp, "SHPCopyVector", SHPCopyVectorProc, 0, 0);
  Tcl_CreateObjCommand(interp, "SHPGetEntityCount", SHPGetEntityCountProc, 0, 0);
  Tcl_CreateObjCommand(interp, "SHPGetVertices", SHPGetVerticesProc, 0, 0);
  Tcl_CreateObjCommand(interp, "SHPGetInfo", SHPGetInfoProc, 0, 0);
  Tcl_CreateObjCommand(interp, "SHPWriteSimpleObject", SHPWriteSimpleObjectProc, 0, 0);
  Tcl_CreateObjCommand(interp, "shapeTouchesBox", shapeTouchesBoxProc, 0, 0);
  Tcl_CreateObjCommand(interp, "utmToLatLon", utmToLatLonProc, 0, 0);
  Tcl_CreateObjCommand(interp, "trimShapes", trimShapesProc, 0, 0);
  Tcl_CreateObjCommand(interp, "mgrsToLatLonZoneXY", mgrsToLatLonZoneXYProc, 0, 0);
  Tcl_CreateObjCommand(interp, "latLonToMgrsZoneXY", latLonToMgrsZoneXYProc, 0, 0);
  return TCL_OK;
}
