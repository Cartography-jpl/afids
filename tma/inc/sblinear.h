#ifndef _sblinear_h_
#define _sblinear_h_

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <fstream>
#if 0
#include <strstream.h>
#endif
#include <iostream>


using namespace std;

//////////////////////////////////////////////////////////////////////////////
//
//  Class: SbVec2f
//
//  2D vector used to represet points or directions. Each component of
//  the vector is a double.
//
//////////////////////////////////////////////////////////////////////////////

class SbVec2f {

  private:
    double vec_[2];

  protected:

  public:

    // Default constructor
    SbVec2f()						{ }

    // Constructor given an array of 2 components
    SbVec2f(const double _v[2])				{ setValue(_v); }

    // Constructor given 2 individual components
    SbVec2f(double _x, double _y)				{ setValue(_x, _y); }

    // Returns dot (inner) product of vector and another vector
    double	dot(const SbVec2f &_v) const;

    // Returns pointer to array of 2 components
    const double	*getValue() const			{ return vec_; }

    // Returns 2 individual components
    void	getValue(double &_x, double &_y) const;

    // Returns geometric length of vector
    double	length() const;

    // Negates each component of vector in place
    void	negate();

    // Changes vector to be unit length
    double	normalize();

    // Sets value of vector from array of 2 components
    SbVec2f &	setValue(const double _v[2]);

    // Sets value of vector from 2 individual components
    SbVec2f &	setValue(double _x, double _y);

    // Accesses indexed component of vector
    double &	  operator [](int _i) 		{ return (vec_[_i]); }
    const double & operator [](int _i) const 	{ return (vec_[_i]); }

	// Lat/lon accessors
	double &      lat()                     { return (vec_[0]);  }
	double &      lon()                     { return (vec_[1]);  }

	// Line/sample accessors
	double &      line()                       { return (vec_[0]);  }
	double &      sample()                     { return (vec_[1]);  }

    // Component-wise scalar multiplication and division operators
    SbVec2f &	operator *=(double _d);

    SbVec2f &	operator /=(double _d)
	{ return *this *= (1.0 / _d); }

    // Component-wise vector addition and subtraction operators
    SbVec2f &	operator +=(const SbVec2f &_u);
    SbVec2f &	operator -=(const SbVec2f &_u);

    // Nondestructive unary negation - returns a new vector
    SbVec2f	operator -() const;

    // Component-wise binary scalar multiplication and division operators
    friend SbVec2f	operator *(const SbVec2f &_v, double _d);
    friend SbVec2f	operator *(double _d, const SbVec2f &_v)
	{ return _v * _d; }
    friend SbVec2f	operator /(const SbVec2f &_v, double _d)
	{ return _v * (1.0 / _d); }

    // Component-wise binary vector addition and subtraction operators
    friend SbVec2f	operator +(const SbVec2f &_v1, const SbVec2f &_v2);

    friend SbVec2f	operator -(const SbVec2f &_v1, const SbVec2f &_v2);

    // Equality comparison operator
    friend int		operator ==(const SbVec2f &_v1, const SbVec2f &_v2);
    friend int		operator !=(const SbVec2f &_v1, const SbVec2f &_v2)
	{ return !(_v1 == _v2); }

    // Equality comparison within given tolerance - the square of the
    // length of the maximum distance between the two vectors
    int		equals(const SbVec2f _v, double _tolerance) const;

    // Output
    friend ostream& operator<<(ostream &_s, const SbVec2f &_sb);

};

//////////////////////////////////////////////////////////////////////////////
//
//  Class: SbVec2s
//
//  2D vector used to represet points or directions. Each component of
//  the vector is a integer.
//
//////////////////////////////////////////////////////////////////////////////

class SbVec2s {
  protected:
    int	vec_[2];		// Storage for vector components

  public:

    // Default constructor
    SbVec2s()						{ }

    // Constructor given an array of 2 components
    SbVec2s(const int _v[2])				{ setValue(_v); }

    // Constructor given 2 individual components
    SbVec2s(int _x, int _y)				{ setValue(_x, _y); }

    // Returns dot (inner) product of vector and another vector
    long	dot(const SbVec2s &_v) const;

    // Returns pointer to array of 2 components
    const int	*getValue() const			{ return vec_; }

    // Returns 2 individual components
    void	getValue(int &_x, int &_y) const;

    // Negates each component of vector in place
    void	negate();

    // Sets value of vector from array of 2 components
    SbVec2s &	setValue(const int _v[2]);

    // Sets value of vector from 2 individual components
    SbVec2s &	setValue(int _x, int _y);

    // Accesses indexed component of vector
    int &	  operator [](int _i) 		{ return (vec_[_i]); }
    const int & operator [](int _i) const 	{ return (vec_[_i]); }

    // Component-wise scalar multiplication and division operators
    SbVec2s &	operator *=(int _d);
    SbVec2s &	operator *=(double _d);

    SbVec2s &	operator /=(int _d);
    SbVec2s &	operator /=(double _d)
	{ return *this *= (1.0 / _d); }

    // Component-wise vector addition and subtraction operators
    SbVec2s &	operator +=(const SbVec2s &_u);
    SbVec2s &	operator -=(const SbVec2s &_u);

    // Nondestructive unary negation - returns a new vector
    SbVec2s	operator -() const;

    // Component-wise binary scalar multiplication and division operators
    friend SbVec2s	operator *(const SbVec2s &_v, int _d);
    friend SbVec2s	operator *(const SbVec2s &_v, double _d);
    friend SbVec2s	operator *(int _d, const SbVec2s &_v)
	{ return _v * _d; }
    friend SbVec2s	operator *(double _d, const SbVec2s &_v)
	{ return _v * _d; }
    friend SbVec2s	operator /(const SbVec2s &_v, int _d);
    friend SbVec2s	operator /(const SbVec2s &_v, double _d)
	{ return _v * (1.0 / _d); }

    // Component-wise binary vector addition and subtraction operators
    friend SbVec2s	operator +(const SbVec2s &_v1, const SbVec2s &_v2);

    friend SbVec2s	operator -(const SbVec2s &_v1, const SbVec2s &_v2);

    // Equality comparison operator
    friend int		operator ==(const SbVec2s &_v1, const SbVec2s &_v2);
    friend int		operator !=(const SbVec2s &_v1, const SbVec2s &_v2)
	{ return !(_v1 == _v2); }

    // Output
    friend ostream& operator<<(ostream &_s, const SbVec2s &_sb);

};

#endif                                                          // _sblinear_h_


