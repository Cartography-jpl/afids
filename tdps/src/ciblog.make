include Makefile.common

OSSIM_INCLUDES  = $(BASE_INCLUDES)
OSSIM_LIBS = $(LINK_OSSIM)

VICAR_ROOT = /usr/local/vicar
VICAR_INCLUDES = -I $(VICAR_ROOT)/p1/inc -I $(VICAR_ROOT)/rtl/inc
VICAR_LIBS = -L $(VICAR_ROOT)/olb/x86-linux -lrtl

MY_INCLUDES = $(OSSIM_INCLUDES) $(VICAR_INCLUDES) \
	-I ./src/ossim_core/imaging/formats/cibcadrg \
	-I ./src/ossim_core/imaging/factory \
	-I ./src/ossim_core/base/data_types \
	-I ./src/ossim_core/imaging \
	-I ./src/ossim_core/imaging/formats \
	-I ./src/ossim_core/imaging/tile_sources

MY_LIBS = $(OSSIM_LIBS) $(VICAR_LIBS) -lstdc++ -lm

ciblog: ciblog.cpp
	gcc -g -o ciblog ciblog.cpp $(MY_INCLUDES) $(MY_LIBS)


