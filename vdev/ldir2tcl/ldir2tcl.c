#include <stdio.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "taeconf.inp"
#include "parblk.inc"
#include "defines.h"
#include <unistd.h>

#include "carto/cartoTaeUtils.h"
#include "carto/cartoVicarProtos.h"

void main44(void)
{
  char path [10000];

  zifmessage("ldir2tcl version Thu Jan  3 2008");

  if (! getwd (path))
    zmabend ("ldir2tcl: getwd failed; perhaps the path is longer than PATH_MAX");
   
  mq_out_string ("strvar", path, 99);

  return;
}
