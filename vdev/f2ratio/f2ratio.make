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

UNIT_NAME = f2ratio
# VICAR program f2ratio

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

CFLAGS = -O

CPPFLAGS =  -I$(V2INC) -I$(TAEINC)    -I$(P1INC)       -I$(P2INC) -I$(P2GENINC)                             -I$(CARTOLIB)/inc

OBJS = f2ratio.o

INCLUDES =

CLEANOBJS = f2ratio.o

CLEANSRC = f2ratio.c

CLEANOTHER =

CLEANINC =

# Note that SYSTEMINC is used only for includes that are installed,
# e.g. from a TAO/IDL build.

 LIBS = -L $(CARTOLIB)/lib -lcarto -L$(V2OLB)              -lp2sub   -lp1sub                                          $(V2OLB)/librtl.a  -lshvic    -L$(TAELIB) $(TAELIB)/libtaec.o  -ltae -lncurses         -lm

std: f2ratio

all: f2ratio doc

system: f2ratio doc clean.obj clean.src

compile:  $(OBJS)

f2ratio:  $(OBJS)
	$(LINK.c) -o f2ratio $(OBJS) $(LIBS)

doc:

clean.obj:
	-$(RM) $(CLEANOBJS)

clean.src:
	-$(RM) $(CLEANSRC) $(CLEANINC) $(CLEANOTHER)
	-$(RM) $(UNIT_NAME).make $(UNIT_NAME).bld $(UNIT_NAME).imake

$(OBJS): $(INCLUDES)

