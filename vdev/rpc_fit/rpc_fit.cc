#include "vicar_raster_image.h"
#include "vicar_dem.h"
#include "vicar_argument.h"
#include "rpc_image.h"
#include "mask_image.h"
#include "memory_raster_image.h"
#include <boost/range/iterator.hpp>
#include <boost/foreach.hpp>

#include <cmath>
#include <cstring>
#include <ctype.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
extern "C" {
void zmabend(char *);
}

using namespace GeoCal;


/************************************************************************/
/* program rpc_fit                                                      */
/************************************************************************/
/*  2008-03 ...alz calling smyth routine ... initial version            */
/************************************************************************/

void main44(void)
{
try{
  zvmessage(const_cast<char*>("rpc_fit version Fri December 5 2008"), 
	    const_cast<char*>(""));

// The number here is how many lines to keep into memory at once. This is an
// empirical number, it shouldn't be so small that we are constantly
// needing to read from disk, and it shouldn't be large enough that we
// are exhausting memory.
  boost::shared_ptr<VicarRasterImage> 
    raw_img(new VicarRasterImage(1, VicarFile::UPDATE, "INP", 500, 8));
  VicarRasterImage ref_img(2, VicarFile::READ, "INP", 500);
  boost::shared_ptr<Dem> dem(new VicarDem(3, false));
  boost::shared_ptr<RasterImage> lw(new VicarRasterImage(4));
  MaskImage mask(lw);
  Rpc rpc = raw_img->vicar_file().rpc();
  BOOST_FOREACH(int i, VicarArgument::arg<std::vector<int> >("RPC_LNUM_FIT"))
    rpc.fit_line_numerator[i] = true;
  BOOST_FOREACH(int i, VicarArgument::arg<std::vector<int> >("RPC_SNUM_FIT"))
    rpc.fit_sample_numerator[i] = true;

  RpcImage new_proj(raw_img, rpc, dem, ref_img.map_info());
  new_proj.fit(ref_img, VicarArgument::arg<double>("MAXDIFF"), mask);
  raw_img->vicar_file().rpc(new_proj.rpc());
} catch(std::exception& e) {
  std::string t = std::string("rpc_fit ERR: ") + e.what();
  zmabend(const_cast<char*>(t.c_str()));
}
}
