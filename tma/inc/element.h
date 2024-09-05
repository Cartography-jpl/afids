#ifndef _element_h_
#define _element_h_

enum Affect {rspeed, vspeed, vdelay, slopeAffect, weather};
// Should match affectMap.  See mapIt().
#define NUM_AFFECTS 5
Affect mapIt(char *_affectStr);

class Element {
 private:
   int id;
   Affect affect;
   double value;
   int index;     // index into terrain raster file.
   double lower;  // lower bound on slope bin.
   double upper;  // upper bound on slope bin.

 protected:

 public:
   Element(int _id, Affect _affect, double _value, int _pgmValue);
   Element(int _id, Affect _affect, double _value,
           double _lower, double _upper);
   Element(int _id, Affect _affect, double _value);
   ~Element();
   int getId();
   Affect getAffect();
   double getValue();
   int getIndex();
   double getLower();
   double getUpper();
                                                                      // input
//   friend istream   &operator >> (istream & _s, Element &_e);
                                                                     // output
   friend ostream &operator<<(ostream &_s, const Element &_e);

}; // Element

#endif                                                          // _element_h_


