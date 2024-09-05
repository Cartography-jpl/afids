BIN = ../bin
LIB = ../lib
INC = ../inc

INCLUDES = -I $(INC) -I /raid1/wlb/latestafids/svn_1154/install/include
LIBS = -L /raid1/wlb/latestafids/svn_1154/install/lib -ltcl8.5 -ltk8.5

tmasrvobjs = \
    appinit.o \
    concave.o \
    contour.o \
    contours.o \
    contoursTcl.o \
    dijkstra.o \
    distanceBetween2Points.o \
    dnet.o \
    element.o \
    loadrnf.o \
    minpath.o \
    minpathTcl.o \
    mover.o \
    queryTerrain.o \
    realwork.o \
    region.o \
    sblinear.o \
    subregion.o \
    util.o

.C.o:
	gcc -Wall -fPIC -g -c $(INCLUDES) $*.C

.c.o:
	gcc -Wall -fPIC -g -c $(INCLUDES) $*.c

all: $(BIN)/pgm2ppm $(BIN)/mintime2contours $(BIN)/overlay2polygons $(BIN)/rasterize $(BIN)/intersect $(BIN)/rnf $(LIB)/libTMASRV.so

$(BIN)/pgm2ppm: pgm2ppm.c Makefile
	gcc -g -Wall -o $(BIN)/pgm2ppm pgm2ppm.c -lm

$(BIN)/mintime2contours: mintime2contours.c Makefile
	gcc -g -Wall -o $(BIN)/mintime2contours mintime2contours.c

$(BIN)/overlay2polygons: overlay2polygons.c Makefile
	gcc -g -Wall -o $(BIN)/overlay2polygons overlay2polygons.c

$(BIN)/rasterize: rasterize.c Makefile $(INC)/gems.h $(INC)/plotline.h plotline.c $(INC)/shiftNscale.h $(INC)/concave.h
	gcc -g -Wall -o $(BIN)/rasterize rasterize.c plotline.c concave.c $(INCLUDES) $(LIBS)

$(BIN)/intersect: Makefile intersect.c $(INC)/intersect.h $(INC)/loadlines.h loadlines.c $(INC)/xlines.h xlines.c
	gcc -g -Wall -o $(BIN)/intersect intersect.c loadlines.c xlines.c $(INCLUDES)

$(BIN)/rnf: Makefile rnf.c $(INC)/rnf.h dnet.c $(INC)/dnet.h pointToSegment.c $(INC)/pointToSegment.h loadrnf.c $(INC)/loadlines.h loadlines.c $(INC)/xlines.h xlines.c $(INC)/shiftNscale.h $(INC)/plotline.h plotline.c
	gcc -g -Wall -o $(BIN)/rnf rnf.c dnet.c pointToSegment.c loadlines.c xlines.c plotline.c loadrnf.c $(INCLUDES) $(LIBS)

$(LIB)/libTMASRV.so: $(tmasrvobjs) Makefile
	gcc -shared -g -o $(LIB)/libTMASRV.so $(tmasrvobjs) -lstdc++ $(LIBS)
