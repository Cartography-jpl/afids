#include <string.h>
#include "tma.h"
#include "element.h"

Affect mapIt(char *_affectStr) {
  const char *affectMap[5] = {"rspeed", "vspeed", "vdelay", "slope", "weather"}; 
  int i;
  for (i=0; i<NUM_AFFECTS; i++)
    if (strcmp(_affectStr, affectMap[i]) == 0)
      return Affect(i);
  cerr << "Error in mapIt(): Invalid affect.\n";
  return rspeed;
}

Element::Element(int _id, Affect _affect, double _value,
                 int _pgmValue) {
  id = _id;
  affect = _affect;
  value = _value;
  index = _pgmValue;
}

Element::Element(int _id, Affect _affect, double _value,
                 double _lower, double _upper) {
  id = _id;
  affect = _affect;
  value = _value;
  lower = _lower;
  upper = _upper;
}

Element::Element(int _id, Affect _affect, double _value) {
  id =  _id;
  affect = _affect;
  value = _value;
}


Element::~Element() {
}

int Element::getId() {
  return id;
}

Affect Element::getAffect() {
  return affect;
}

double Element::getValue() {
  return value;
}

int Element::getIndex() {
  return index;
}

double Element::getLower() {
  return lower;
}

double Element::getUpper() {
  return upper;
}

ostream &operator<<(ostream &_s, const Element &_e)
{
  return _s << "Element Information" << endl << 
                        "Name = "   << _e.id << endl <<
			"Affect = " << _e.affect << endl <<
			"Value = "  << _e.value << endl;
}


