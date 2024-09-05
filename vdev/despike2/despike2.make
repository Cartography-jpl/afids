# Makefile generated by imake - do not edit!
# $Xorg: imake.c,v 1.6 2001/02/09 02:03:15 xorgcvs Exp $

#***********************************************************************
#
# This makefile is automatically generated by imake.  If you need to change
# the makefile, edit the .imake file and re-run imake, or contact the VICAR
# system programmer to change the imake templates.
#
# *** DO NOT EDIT THIS FILE!! ***
#
#***********************************************************************

#

UNIT_NAME = despike2
# VICAR program despike2

#
#***********************************************************************

CC = gcc -Wall

SHELL = /bin/sh
CP = cp
CHMOD_X = chmod +x

AR = ar r

ARX = ar x

RM = rm -f
LN = ln -fs

RANLIB = ranlib

RUN_TM  = taetm
MSGBLD  = $$TAEPDF/msgbld

LINK.c = $(CC)  $(CFLAGS) $(CPPFLAGS) $(LDFLAGS)

.c.o:
	$(CC) $(CFLAGS) $(CPPFLAGS) -c  $<

LDFLAGS =

CFLAGS = -g

CPPFLAGS =  -I$(V2INC) -I$(TAEINC)    -I$(P1INC)       -I$(P2INC) -I$(P2GENINC)                          -I$(GSLLIB)/inc -I$(GSLLIB)/gsl-1.9   -I$(CARTOLIB)/inc

OBJS = despike2.o ImageUtils.o

INCLUDES =

CLEANOBJS = despike2.o ImageUtils.o

CLEANSRC = despike2.c ImageUtils.c

CLEANOTHER =

CLEANINC =

# Note that SYSTEMINC is used only for includes that are installed,
# e.g. from a TAO/IDL build.

 LIBS = -L $(CARTOLIB)/lib -lcarto -L$(V2OLB)              -lp2sub   -lp1sub                           $(GSLLIB)/lib/libgsl.a $(GSLLIB)/gsl-1.9/blas/.libs/libgslblas.a $(GSLLIB)/gsl-1.9/cblas/.libs/libgslcblas.a               $(V2OLB)/librtl.a  -lshvic    -L$(TAELIB) $(TAELIB)/libtaec.o  -ltae -lncurses         -lm

std: despike2

all: despike2 doc

system: despike2 doc clean.obj clean.src

compile:  $(OBJS)

despike2:  $(OBJS)
	$(LINK.c) -o despike2 $(OBJS) $(LIBS)

doc:

clean.obj:
	-$(RM) $(CLEANOBJS)

clean.src:
	-$(RM) $(CLEANSRC) $(CLEANINC) $(CLEANOTHER)
	-$(RM) $(UNIT_NAME).make $(UNIT_NAME).bld $(UNIT_NAME).imake

$(OBJS): $(INCLUDES)

