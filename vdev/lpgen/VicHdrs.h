#ifndef VICHDRS_H
#define VICHDRS_H

#ifndef ZVPROTO_H
#define ZVPROTO_H
#include "zvproto.h"
#endif

void zmabend(char *errorMsg);
char toupper(char c);
int IBISColumnGet(int, char*, char*, int);
int IBISColumnSet(int, char*, char*, int);
int IBISColumnWrite(int, void*, int, int, int);
void zifmessage(char *);

#endif
