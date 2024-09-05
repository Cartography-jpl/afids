#include <string.h>
#include <math.h>

#include "tma.h"
#include "util.h"
#include "subregion.h"
#include "element.h"
#include "mover.h"
#include "tcl.h"


///////////////////////////////////////////////////////////////////////////
//
// Constructor using moverType string.
//
///////////////////////////////////////////////////////////////////////////
Mover::Mover(const char *regionName, const char *moverType, const char *dataSet) {

  // Read elements for mover type.
  // Mover data is in tempMoverData, and is temporarily created from the database.
  // Format of lines in file are :
  //   id affect value [pgmValue] [lower] [upper].

  char filename[512];
  // Get terrain filename from database using Data Layer.
  Tcl_Interp *interp = getInterp();
 
  char script[1000];
  sprintf(script, "genMover %s %s %s", getenv("TMA_APPLICATION_ID"), dataSet, moverType);
  fprintf(stderr, "%s\n", script);
  int code = Tcl_Eval(interp, script);

  if (code != TCL_OK) {
    fprintf(stderr, "%s\n", interp->result);
    fprintf(stderr, "Error in:\n%s\n", script);
    exit(1);
  }

  sprintf(filename, "%s/tempMoverData", getenv("TMA_DATA"));
  cerr << "Reading " << filename << " ...\n";
  FILE *filePtr = fopen(filename, "r");
  if (filePtr == NULL) {
    cerr << "Could not open " << filename << endl;
    exit(1);
  }
  fscanf(filePtr, "%d", &numElements);
  elements = new Element*[numElements];
  int i;
  int elementId;
  char affectStr[255];
  Affect affect;
  float value;
  int pgmValue;
  float lower, upper;
  for (i=0; i<numElements; i++) {
    fscanf(filePtr, "%d %s %f", &elementId, affectStr, &value);
    //cerr << "elementId = " << elementId << endl;
    //cerr << "affectStr = " << affectStr << endl;
    //cerr << "value = " << value << endl;
    affect = mapIt(affectStr);
    switch (affect) {
      case rspeed:
        //cerr << "rspeed\n";
        fscanf(filePtr, "%d", &pgmValue);
        elements[i] = new Element(elementId, affect, (double)value,
                                  pgmValue);
        break;
      case slopeAffect:
        //cerr << "slopeAffect\n";
        fscanf(filePtr, "%f %f", &lower, &upper);
        cerr << "lower << " << lower << ", upper = " << upper << endl;
        elements[i] = new Element(elementId, affect, (double)value,
                                  (double)lower, (double)upper);
        break;
      case vspeed: case vdelay: case weather:
        //cerr << "vspeed, vdelay, weather\n";
        elements[i] = new Element(elementId, affect, (double)value);
        break;
      default:
        cerr << "Bad affect in mover data\n";
    }
  }
  fclose(filePtr);
}

///////////////////////////////////////////////////////////////////////////
//
// Destructor.
//
///////////////////////////////////////////////////////////////////////////
Mover::~Mover() {
  int i;
  for (i=0; i<numElements; i++)
    delete elements[i];
  delete [] elements;
}

///////////////////////////////////////////////////////////////////////////
//
// Returns value for element.
//
///////////////////////////////////////////////////////////////////////////
double Mover::getValue(int elementId) {
  int eIndex;
  for (eIndex=0; eIndex<numElements; eIndex++)
     if (elements[eIndex]->getId() == elementId)
       return elements[eIndex]->getValue();
  return 0.0;
}

///////////////////////////////////////////////////////////////////////////
//
// Returns element.
//
///////////////////////////////////////////////////////////////////////////
Element &Mover::getElement(const int i) {
  if (i>=numElements) {
    cerr << "Error in Mover::getElement(): range error." << endl;
    return *(elements[0]);
  }
  else
    return *(elements[i]);
}

///////////////////////////////////////////////////////////////////////////
//
// Returns number of elements.
//
///////////////////////////////////////////////////////////////////////////
int Mover::getNumElements() {
  return numElements;
}


