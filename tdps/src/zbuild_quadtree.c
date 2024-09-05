#include <stdio.h>
#include <stdlib.h>
#include "QuadtreeBuilder.h"

main(int argc, char **argv)
{
   DEMPtr dem;
   QuadtreePtr tree;
   FILE *file;
   
   if ( argc < 2 )
   {
     fprintf(stderr, 
 "Insufficient number of arguments (file name (i.e. KoreaLittleEndean.dat))\n");
     return 0;
   }

   dem = loadDEM();

   tree = buildQuadtree(dem);

   displayTreeHeader(tree);

   file = fopen(argv[1], "wb");
   
   saveQuadtree(file, tree, LITTLE_ENDEAN);

   fclose(file);

   printf("\n\nFinished Building Tree File\n\n"); 

}

