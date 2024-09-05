INC = /home/wbunch/afids_1209_rh6/install/include
LIB = /home/wbunch/afids_1209_rh6/install/lib

OSSIM_INCLUDES  = \
	-I $(INC) \
	-I $(INC)/carto \
	-I $(INC)/ossim/base \
	-I $(INC)/ossim/imaging \
	-I $(INC)/ossim/vec \
	-I $(INC)/ossim/support_data

OSSIM_LIBS = $(LIB)/libossim.so $(LIB)/libgeotiff.so.2 $(LIB)/libOpenThreads.so.0 $(LIB)/shapeUtils.so $(LIB)/libtcl8.5.so $(LIB)/libz.so

MY_INCLUDES = $(OSSIM_INCLUDES)

vmap2shp: vmap2shp.cpp
	gcc -Wall -g -o vmap2shp vmap2shp.cpp $(MY_INCLUDES) $(OSSIM_LIBS) -lstdc++
