#ifndef _mover_h_
#define _mover_h_

#include <iostream>

#include "element.h"

class Mover {
 private:
   Element **elements;
   int numElements;

 protected:

 public:
   Mover(const char *regionName, const char *moverType, const char *dataPath);
   ~Mover();
   int getNumElements();
   Element &getElement(const int);
   double getValue(int elementId);
                                                                    // output
   friend ostream &operator<<(ostream &_s, const Mover &_m);

}; // Mover

#endif                                                          // _mover_h_


