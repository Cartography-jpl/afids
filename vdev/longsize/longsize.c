#include <stdio.h>

#include "vicmain_c.h"
#include "applic.h"
#include "taeconf.inp"
#include "parblk.inc"
#include "defines.h"

#include "carto/cartoVicarProtos.h"
#include "carto/cartoTaeUtils.h"

void main44(void)
{
  zifmessage( "longsize version Thu May  8 2008" );
   
  mq_out_int ( "intvar", sizeof( long ) );
}
