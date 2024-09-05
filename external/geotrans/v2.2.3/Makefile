####
# Compiler declarations
####

#Sun SPARCworks
cc = cc
#CC = CC
#Gnu
#cc = gcc
#CC = g++

####
# Support declarations
####

#DEBUG = -g
#WARNINGS = -Wall

CFLAGS = $(DEBUG) $(WARNINGS)

####
# Libraries
####

LIB =  libdtcc.a

####
# Sources
####

SRCS = \
	albers.c \
	azeq.c \
	bonne.c \
	bng.c \
	cassini.c \
	cyleqa.c \
	datum.c \
	eckert4.c \
	eckert6.c \
	ellipse.c \
	eqdcyl.c \
	geocent.c \
	geoid.c \
	georef.c \
	gnomonic.c \
	grinten.c \
        lambert.c \
	loccart.c \
	mercator.c \
	mgrs.c \
	miller.c \
	mollweid.c \
	neys.c \
	nzmg.c \
	omerc.c \
	orthogr.c \
	polarst.c \
	polycon.c \
	sinusoid.c \
	stereogr.c \
	trcyleqa.c \
	tranmerc.c \
	ups.c \
	utm.c

####
# Suffix rules
####

.SUFFIXES: .c .o

.c.o:
	$(cc) -c $(CFLAGS) $<

####
# Targets
####

all : $(LIB)

$(SRCS):
	ln -s $(DTCCDIR)/$(@:.c=)/$@ .

$(LIB): $(SRCS:.c=.o)
	ar -rv $(LIB) $(SRCS:.c=.o)

clean:
	$(RM) *.o
