
#include "sblinear.h"

//////////////////////////////////////////////////////////////////////////////
//
//  Class: SbVec2f
//
//  2D vector used to represet points or directions. Each component of
//  the vector is a double.
//
//////////////////////////////////////////////////////////////////////////////


// Returns dot (inner) product of vector and another vector
double	SbVec2f::dot(const SbVec2f &_v) const {
  return vec_[0]*_v[0] + vec_[1]*_v[1];
}

// Returns 2 individual components
void	SbVec2f::getValue(double &_x, double &_y) const {
  _x = vec_[0];
  _y = vec_[1];
}

// Returns geometric length of vector
double	SbVec2f::length() const {
  return sqrt(vec_[0]*vec_[0] + vec_[1]*vec_[1]);
}

// Negates each component of vector in place
void	SbVec2f::negate() {
  vec_[0] *= -1.0;
  vec_[1] *= -1.0;
}

// Changes vector to be unit length
double	SbVec2f::normalize() {
  double length = sqrt(vec_[0]*vec_[0] + vec_[1]*vec_[1]);
  if (length > 0.0) {
    vec_[0] /= length;
    vec_[1] /= length;
  }
  return length;
}

// Sets value of vector from array of 2 components
SbVec2f &	SbVec2f::setValue(const double _v[2]) {
  return this->setValue(_v[0], _v[1]);
}

// Sets value of vector from 2 individual components
SbVec2f &	SbVec2f::setValue(double _x, double _y) {
  vec_[0] = _x;
  vec_[1] = _y;
  return *this;
}

// Component-wise scalar multiplication and division operators
SbVec2f &	SbVec2f::operator *=(double _d) {
  vec_[0] *= _d;
  vec_[1] *= _d;
  return *this;
}

// Component-wise vector addition and subtraction operators
SbVec2f &	SbVec2f::operator +=(const SbVec2f &_u) {
  vec_[0] += _u[0];
  vec_[1] += _u[1];
  return *this;
}

SbVec2f &	SbVec2f::operator -=(const SbVec2f &_u) {
  vec_[0] -= _u[0];
  vec_[1] -= _u[1];
  return *this;
}

// Nondestructive unary negation - returns a new vector
SbVec2f	SbVec2f::operator -() const {
  return -1.0*SbVec2f(this->getValue());
}

// Component-wise binary scalar multiplication and division operators
SbVec2f	operator *(const SbVec2f &_v, double _d) {
  return SbVec2f(_v[0]*_d, _v[1]*_d);
}

// Component-wise binary vector addition and subtraction operators
SbVec2f	operator +(const SbVec2f &_v1, const SbVec2f &_v2) {
  return SbVec2f(_v1[0]+_v2[0], _v1[1]+_v2[1]);
}

SbVec2f	operator -(const SbVec2f &_v1, const SbVec2f &_v2) {
  return SbVec2f(_v1[0]-_v2[0], _v1[1]-_v2[1]);
}

// Equality comparison operator
int operator ==(const SbVec2f &_v1, const SbVec2f &_v2) {
  return ((_v1[0]==_v2[0]) && (_v1[1]==_v2[1]));
}

// Equality comparison within given tolerance - the square of the
// length of the maximum distance between the two vectors
int SbVec2f::equals(const SbVec2f _v, double _tolerance) const {  
  return ( vec_[0]*_v[0] + vec_[1]*_v[1] < _tolerance );
}

// Output
ostream &operator<<(ostream &_s, const SbVec2f &_sb)
{
  return _s << "(" << _sb.vec_[0] << ", " << _sb.vec_[1] << ")" << endl;
}



//////////////////////////////////////////////////////////////////////////////
//
//  Class: SbVec2s
//
//  2D vector used to represet points or directions. Each component of
//  the vector is a integer.
//
//////////////////////////////////////////////////////////////////////////////

// Returns dot (inner) product of vector and another vector
long	SbVec2s::dot(const SbVec2s &_v) const {
  return long(vec_[0])*long(_v[0]) + long(vec_[1])*long(_v[1]);
}

// Returns 2 individual components
void	SbVec2s::getValue(int &_x, int &_y) const {
  _x = vec_[0];
  _y = vec_[1];
}

// Negates each component of vector in place
void	SbVec2s::negate() {
  vec_[0] *= -1;
  vec_[1] *= -1;
}

// Sets value of vector from array of 2 components
SbVec2s &	SbVec2s::setValue(const int _v[2]) {
  return this->setValue(_v[0], _v[1]);
}

// Sets value of vector from 2 individual components
SbVec2s &	SbVec2s::setValue(int _x, int _y) {
  vec_[0] = _x;
  vec_[1] = _y;
  return *this;
}

// Component-wise scalar multiplication and division operators
SbVec2s &	SbVec2s::operator *=(int _d) {
  vec_[0] *= _d;
  vec_[1] *= _d;
  return *this;
}

SbVec2s &	SbVec2s::operator *=(double _d) {
  vec_[0] *= int(_d);
  vec_[1] *= int(_d);
  return *this;
}

SbVec2s &	SbVec2s::operator /=(int _d) {
  vec_[0] /= int(_d);
  vec_[1] /= int(_d);
  return *this;
}

// Component-wise vector addition and subtraction operators
SbVec2s &	SbVec2s::operator +=(const SbVec2s &_u) {
  vec_[0] += _u[0];
  vec_[1] += _u[1];
  return *this;
}

SbVec2s &	SbVec2s::operator -=(const SbVec2s &_u) {
  vec_[0] -= _u[0];
  vec_[1] -= _u[1];
  return *this;
}

// Nondestructive unary negation - returns a new vector
SbVec2s	SbVec2s::operator -() const {
  return -1.0*SbVec2s(this->getValue());
}

// Component-wise binary scalar multiplication and division operators
SbVec2s	operator *(const SbVec2s &_v, int _d) {
  return SbVec2s(_v[0]*_d, _v[1]*_d);
}

SbVec2s	operator *(const SbVec2s &_v, double _d) {
  return SbVec2s(_v[0]*int(_d), _v[1]*int(_d));
}

SbVec2s	operator /(const SbVec2s &_v, int _d) {
  return SbVec2s(_v[0]/int(_d), _v[1]/int(_d));
}

// Component-wise binary vector addition and subtraction operators
SbVec2s	operator +(const SbVec2s &_v1, const SbVec2s &_v2) {
  return SbVec2s(_v1[0]+_v2[0], _v1[1]+_v2[1]);
}

SbVec2s	operator -(const SbVec2s &_v1, const SbVec2s &_v2) {
  return SbVec2s(_v1[0]-_v2[0], _v1[1]-_v2[1]);
}

// Equality comparison operator
int operator ==(const SbVec2s &_v1, const SbVec2s &_v2) {
  return ((_v1[0]==_v2[0]) && (_v1[1]==_v2[1]));
}

// Output
ostream &operator<<(ostream &_s, const SbVec2s &_sb)
{
  return _s << "(" << _sb.vec_[0] << ", " << _sb.vec_[1] << ")" << endl;
}
