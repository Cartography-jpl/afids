TCL_INCLUDES = -I $(V2EXTERNAL)/tcltk/v8.4/x86-linux/include
TCL_LIBS = -L $(V2EXTERNAL)/tcltk/v8.4/x86-linux/lib -ltcl8.4 -ltk8.4 -lpthread

TIFF_LIB = -L $(V2EXTERNAL)/tiff/v3.8/x86-linux/lib -ltiff
TIFF_INCLUDES = -I $(V2EXTERNAL)/tiff/v3.8/x86-linux/include
GEOTIFF_LIB = -L $(V2EXTERNAL)/tiff/v3.8/x86-linux/tiff/lib -lgeotiff
Z_LIB =  -L $(V2EXTERNAL)/tiff/v3.8/x86-linux/lib -lz

MY_INCLUDES = $(TCL_INCLUDES) $(TIFF_INCLUDES)
MY_LIBS = $(TCL_LIBS) $(TIFF_LIB) $(GEOTIFF_LIB) $(Z_LIB)

all: tiffTcl.so

tiffTcl.o: tiffTcl.c
	gcc -DAFIDS_GEOTIFF -g -c -fPIC -o tiffTcl.o tiffTcl.c $(MY_INCLUDES)

tiffTcl.so: tiffTcl.o
	gcc -Wall -shared -o tiffTcl.so tiffTcl.o $(MY_LIBS)
